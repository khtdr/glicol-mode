;;; glicol-docs.el --- Documentation for Glicol nodes -*- lexical-binding: t -*-

;;; Commentary:
;; Documentation data for Glicol nodes and their parameters

;;; Code:

(defconst glicol-node-docs
  '((sin . ((description . "sine wave oscillator")
            (parameters . ((freq . "Modulable(440.0)")))
            (input . "none")
            (output . "sine wave float number streams, ranging from -1.0 to 1.0")
            (example . "o: sin 440\n// fm_example: sin ~freq\n// ~freq: sin 20 >> mul 100 >> add 300")))
    
    (saw . ((description . "sawtooth wave oscillator")
            (parameters . ((freq . "Modulable(440.0)")))
            (input . "none")
            (output . "sawtooth wave float number streams, ranging from -1.0 to 1.0")
            (example . "o: saw 440\n// fm_example: saw ~freq\n// ~freq: sin 20 >> mul 100 >> add 300")))
    
    (squ . ((description . "square wave oscillator")
            (parameters . ((freq . "Modulable(440.0)")))
            (input . "none")
            (output . "square wave float number streams, ranging from -1.0 to 1.0")
            (example . "o: squ 440\n// fm_example: squ ~freq\n// ~freq: sin 20 >> mul 100 >> add 300")))
    
    (tri . ((description . "triangle wave oscillator")
            (parameters . ((freq . "Modulable(440.0)")))
            (input . "none")
            (output . "triangle wave float number streams, ranging from -1.0 to 1.0")
            (example . "o: tri 440\n// fm_example: tri ~freq\n// ~freq: sin 20 >> mul 100 >> add 300")))
    
    (mul . ((description . "multiply a number to the input signal and send the signal as ouput")
            (parameters . ((value . "Modulable(1.0)")))
            (input . "a signal")
            (output . "the modified signal")
            (example . "o: sin 440.0 >> mul 0.1")))
    
    (add . ((description . "add a number to the input signal and send the signal as ouput")
            (parameters . ((value . "Modulable(1.0)")))
            (input . "a signal")
            (output . "the modified signal")
            (example . "o: sin 440.0 >> mul ~mod\n~mod: sin 0.1 >> mul 0.3 >> add 0.5")))
    
    (noiz . ((description . "noise signal generator")
             (parameters . ((seed . "NonModulable(42)")))
             (input . "none")
             (output . "noise float number streams, ranging from -1.0 to 1.0")
             (example . "o: noiz 42 >> mul 0.1")))
    
    (imp . ((description . "impulse signal generator")
            (parameters . ((freq . "NonModulable(1.0)")))
            (input . "none")
            (output . "impulse signal like [1.0, 0.0, 0.0, ...]; the gap between two 1.0 depends on the frequency.")
            (example . "o: imp 1.0")))
    
    (lpf . ((description . "resonate low pass filter:\ncut off the high frequency part based on cutoff_freq")
            (parameters . ((cutoff_freq . "Modulable(100.0)")
                         (q_value . "NonModulable(1.0)")))
            (input . "a signal to be filtered")
            (output . "filtered signal")
            (example . "o: saw 440.0 >> lpf 100.0 1.0")))
    
    (hpf . ((description . "resonate high pass filter:\ncut off the low frequency part based on cutoff_freq")
            (parameters . ((cutoff_freq . "Modulable(100.0)")
                         (q_value . "NonModulable(1.0)")))
            (input . "a signal to be filtered")
            (output . "filtered signal")
            (example . "o: saw 440.0 >> hpf 1000.0 1.0")))
    
    (envperc . ((description . "an percussive shape envelope")
                (parameters . ((attack . "NonModulable(0.01)")
                             (decay . "NonModulable(0.1)")))
                (input . "any non-zero value will trigger the envelope. the value will determine the scale of the envelope.")
                (output . "the envelope shape in time.")
                (example . "o: sin 100 >> mul ~env; ~env: imp 1.0 >> envperc 0.01 0.1;")))
    
    (seq . ((description . "unlimited number of midi notes seperated by spaces.\nunderscore means rest.\ncompound note consists of underscrores and midi notes.\none bar will first divided based on spaces.\nthen each compound note will further be divided equally.")
            (parameters . ((seq . "Seq")))
            (input . "only use 'speed' node for 'seq' node input")
            (output . "relative pitch converted by the midi notes (60=>1.0; 72=>2.0; 48=>0.5).\nthis pitch float will only come once based on the timing in pattern.\nthe rest of the float stream will be zeroes.")
            (example . "o: speed 1.0 >> seq 72 _72 _48 60__60 >> sawsynth 0.001 0.05 \n>> lpf 300.0 1.0")))
    
    (choose . ((description . "choose a number from the provided sequence.")
               (parameters . ((seq . "Seq")))
               (input . "no input")
               (output . "one number one block")
               (example . "")))
    
    (sp . ((description . "playback the sample.")
           (parameters . ((sampleName . "Symbol")))
           (input . "any none zero value will trigger the sample playback.\nfor eaxample, 1.0 triggers default pitch, 0.5 octave lower.")
           (output . "the sample playback audio streams.")
           (example . "load_samples_first_in_console: speed 1.0 >> seq 72 _60 _ _55_60 >> sp \\808bd_0")))
    
    (speed . ((description . "use before the seq to control the seq speed.")
              (parameters . ((speed . "NonModulable(1.0)")))
              (input . "no input")
              (output . "the speed at the first place of the block; the rest of the block will be zeros")
              (example . "")))
    
    (bd . ((description . "synthesized kick drum. only decay is tweakable for simplicity.")
           (parameters . ((decay . "NonModulable(0.3)")))
           (input . "value larger than 0 will triggers the envelope and determines the amp")
           (output . "the kick drum sound")
           (example . "out: seq 60 60 60 60 >> bd 0.3")))
    
    (sn . ((description . "synthesized snare drum. only decay is tweakable for simplicity.")
           (parameters . ((decay . "NonModulable(0.3)")))
           (input . "value larger than 0 will triggers the envelope and determines the amp")
           (output . "the snare drum sound")
           (example . "a: seq 60 _ 60 _ >> bd 0.1; b: seq _ 60 _ 60 >> sn 0.1 >> mul 0.5")))
    
    (hh . ((description . "synthesized hi hat. only decay is tweakable for simplicity.")
           (parameters . ((decay . "NonModulable(0.3)")))
           (input . "value larger than 0 will triggers the envelope and determines the amp")
           (output . "the hi hat sound")
           (example . "c: speed 8.0 >> seq 60 >> hh 0.03")))
    
    (sawsynth . ((description . "a minimal synth based on sawtooth wave.")
                 (parameters . ((attack . "NonModulable(0.01)")
                              (decay . "NonModulable(0.1)")))
                 (input . "value larger than 0 will triggers the envelope and determines the pitch (1.0: middle C, 2.0 octave higher)")
                 (output . "the synth signal; range determined by the envelope")
                 (example . "out: seq 33 _33 _33 _33 >> sawsynth 0.01 0.1")))
    
    (squsynth . ((description . "a minimal synth based on square wave.")
                 (parameters . ((attack . "NonModulable(0.01)")
                              (decay . "NonModulable(0.1)")))
                 (input . "value larger than 0 will triggers the envelope and determines the pitch (1.0: middle C, 2.0 octave higher)")
                 (output . "the synth signal; range determined by the envelope")
                 (example . "out: seq 33 _33 _33 _33 >> squsynth 0.01 0.1")))
    
    (trisynth . ((description . "a minimal synth based on triangle wave.")
                 (parameters . ((attack . "NonModulable(0.01)")
                              (decay . "NonModulable(0.1)")))
                 (input . "value larger than 0 will triggers the envelope and determines the pitch (1.0: middle C, 2.0 octave higher)")
                 (output . "the synth signal; range determined by the envelope")
                 (example . "out: seq 33 _33 _33 _33 >> trisynth 0.01 0.1")))
    
    (plate . ((description . "a fixed Dattorro plate reverb.")
              (parameters . ((mix . "NonModulable(0.1)")))
              (input . "dry signal")
              (output . "wet signal with the reverb effect")
              (example . "out: seq 60 _60 _60 _60 >> sp \\sid >> plate 0.1")))
    
    (balance . ((description . "get two mono signal and put it into left and right of a stereo signal")
                (parameters . ((l . "Reference")
                             (r . "Reference")))
                (input . "two mono signals as left and right")
                (output . "the mixed signal")
                (example . "~left: squ 100 >> mul 0.1; ~right: noiz 42 >> mul 0.1; balance ~left ~right")))
    
    (delayms . ((description . "delay the input signal by some millisecond")
                (parameters . ((delay_time_ms . "Modualable(0.1)")))
                (input . "the signal to be delayed")
                (output . "delayed signal")
                (example . "a: imp 1\nb: imp 1 >> delayms ~mod >> mul 0.6\n~mod: sin 0.2 >> mul 100 >> add 150")))
    
    (delayn . ((description . "delay the input signal by certain samples")
               (parameters . ((delay_samples . "NonModualable(1)")))
               (input . "the signal to be delayed")
               (output . "delayed signal")
               (example . "a: imp 1\nb: imp 1 >> delayn 4000 >> mul 0.6")))
    
    (apfgain . ((description . "an allpass filter with delay in millisecond and feedback gain control")
                (parameters . ((delay_time_ms . "Modualable(0.1)")
                             (gain . "NonModualable(0.9)")))
                (input . "the signal to be filtered")
                (output . "filtered signal")
                (example . "")))
    
    (mix . ((description . "mix all the input signal as is")
            (parameters . ((ref_list . "RefList")))
            (input . "a sequence of references as inputs;\nbetter to use .. for ref loose match")
            (output . "mixed signal")
            (example . "~t1: sin 440;\n~t2: sin 330;\n~d1: sin 550;\nout: mix ~t.. ~d.. >> mul 0.3")))
    
    (meta . ((description . "create a meta node for sample-level control")
             (parameters . ((rhai_script . "Code")))
             (input . "optional, can be any signal but only 1 channel will be used")
             (output . "customised output; should pad at least 128 numbers for output")
             (example . "o: sin 100 >> meta `output = input.map(|x|x*0.1);output`\n// more on https://glicol.org/tour#meta1")))
    
    (onepole . ((description . "an one pole low pass filter")
                (parameters . ((filter_ratio . "Modualble(0.1)")))
                (input . "the signal to be filtered")
                (output . "filtered signal")
                (example . "o: saw 100 >> onepole 0.01")))))

(provide 'glicol-docs)
;;; glicol-docs.el ends here
