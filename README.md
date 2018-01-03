Deep Learning of Narrative
===========================

- Input docs as json lines
  - lazy stream or one line/doc at a time? latter done.
   
- tokenize
    - capitalization (char-rnn annotation trick?), punctuation, numbers, 
    - light touch done. Going with word-RNN for now. 

- Frames TODO
	- output direclty to frametrain if using sdr pro-tem re-engineer later. 
	- What does text2vec do? 
  
- EMBEDDINGS text2vec vs. SDR
	- why is text2vec hand-wavy about semantic locality
	- SDR is clear should we use SDR? downsampling SDRs/or sparse rep as they are very big input space
	- what does text2vec do?

- TERMS rare vs. common words filtering/folding?

- LSTM word sequence model
	- char-RNN vs. word-RNN word on the street is word for performance and profit.
    - training time on example is a wee bit pedestrian on e.g. Shakespeare corpus but
      we only do that once.
    - could just chuck some other narrative at it and see what we get
      it's good way of evidencing the language model.
    - will need to mess with hyper-parameters and network design of course.



   
