use ndarray::{Array, Array1};
use ndarray_linalg::Norm;
use ort::execution_providers::CoreMLExecutionProvider;
use ort::session::builder::GraphOptimizationLevel;
use ort::value::Tensor;
use tokenizers::Tokenizer;

pub struct Inference {
    tokenizer: Tokenizer,
    ort_session: ort::session::Session,
}

#[derive(Debug)]
pub enum InferenceError {
    TokenizerError(tokenizers::Error),
    OrtError(ort::Error),
    NdarrayError(ndarray::ShapeError),
}

pub type Embedding = Array1<f32>;

impl Inference {
    pub fn init() -> Result<(), InferenceError> {
        ort::init()
            .with_execution_providers([CoreMLExecutionProvider::default().build()])
            .commit()?;

        Ok(())
    }

    pub fn new() -> Result<Self, InferenceError> {
        let ort_session = ort::session::Session::builder()?
            .with_optimization_level(GraphOptimizationLevel::Level2)?
            .with_intra_threads(1)?
            .commit_from_memory(include_bytes!("./model.onnx"))?;

        Ok(Inference {
            tokenizer: Tokenizer::from_bytes(include_bytes!("./tokenizer.json"))?,
            ort_session,
        })
    }

    pub fn embed(&self, text: &str) -> Result<Embedding, InferenceError> {
        let encoding = self.tokenizer.encode(text, true)?;
        let ids = encoding.get_ids();
        let attention = encoding.get_attention_mask();

        let id_tensor = Tensor::from_array((
            [1, ids.len()],
            ids.iter().map(|x| *x as i64).collect::<Vec<_>>(),
        ))?;

        let attention_tensor = Tensor::from_array((
            [1, attention.len()],
            attention.iter().map(|x| *x as i64).collect::<Vec<_>>(),
        ))?;

        let result = self
            .ort_session
            .run(ort::inputs![id_tensor, attention_tensor]?)?;

        let output = result[0].try_extract_tensor::<f32>()?;
        let att = Array::from_shape_vec(
            [1, attention.len()],
            attention.iter().map(|x| *x as f32).collect::<Vec<_>>(),
        )?;

        let mut mean = Array1::<f32>::zeros([768]);
        for (token_idx, weight) in attention.iter().enumerate() {
            let token = output
                .slice(ndarray::s![0, token_idx, ..])
                .mapv(|v| v * (*weight as f32));
            mean += &token;
        }
        mean /= att.sum();

        let norm = mean.norm();
        return Ok(mean / norm);
    }
}

impl From<tokenizers::Error> for InferenceError {
    fn from(err: tokenizers::Error) -> Self {
        InferenceError::TokenizerError(err)
    }
}

impl From<ort::Error> for InferenceError {
    fn from(err: ort::Error) -> Self {
        InferenceError::OrtError(err)
    }
}

impl From<ndarray::ShapeError> for InferenceError {
    fn from(err: ndarray::ShapeError) -> Self {
        InferenceError::NdarrayError(err)
    }
}
