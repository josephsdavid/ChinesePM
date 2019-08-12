import tensorflow as tf
import keras
from keras.models import load_model
from keras.utils import CustomObjectScope
from keras.initializers import glorot_uniform


with CustomObjectScope({'GlorotUniform': glorot_uniform()}):
        model = load_model('../analysis/daily/winner.h5')
#tf.keras.utils.plot_model(model, to_file='model.png')

