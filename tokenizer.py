#!/usr/bin/env python3
"""
    Text procesing module: text frame tokenizer
"""
import functools
import string
import unicodedata
import argparse
import re
import sys
import types
import json

@functools.lru_cache(maxsize=1)
def unicode_punctuation_map():
    return dict.fromkeys(
        i for i in range(sys.maxunicode) if unicodedata.category(chr(i)).startswith('P'))

@functools.lru_cache(maxsize=1)
def unicode_punctuation():
    return "".join([chr(i) for i in range(sys.maxunicode) if unicodedata.category(chr(i)).startswith('P')])


def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        pass

    try:
        unicodedata.numeric(s)
        return True
    except (TypeError, ValueError):
        pass

    return re.search(r'[0-9]+/[0-9]+', s)



def has_longrun(s):
    """Does string contain three or more repeated chars?"""
    return re.search(r'(.)\1{2,}', s)


def clean(t):
    """Only sensible alpha"""
    return (len(t) > 1 and not is_number(t) and not has_longrun(t))

def nofilter(t):
    return True


def minimal(t):
    return (len(t) > 1 and not has_longrun(t))


FILTERS = {
    "is_number": is_number,
    "has_longrun": has_longrun,
    "clean": clean,
    "minimal": minimal,
    "nofilter": nofilter,
    "none": nofilter
    }
    
def filter(s) :
    return FILTERS.get(s, None)


# unicode version of EOS?
EOS = r'(?<=[^A-Z].[.?!]) +(?=[A-Z"])'


def sentence_split(s):
    return re.split(EOS, s)


def minisplit(pattern, string):
    splits = list((m.start(), m.end()) for m in re.finditer(pattern, string))
    #print(splits)
    starts = [0] + [i[1] for i in splits[0:-1]]
    ends = [i[0] for i in splits]
    # is there a tail?
    lastsplit = splits[-1][1] if len(splits) > 0 else 0
    end = len(string)
    tail = string[lastsplit:end] if lastsplit < end else ''
    return [string[s:e] for s, e in zip(starts, ends)], tail

#
#def zerowidthsplit(pattern, string):
#    splits = list(m.start() for m in re.finditer(pattern, string))
#    starts = [0] + splits
#    ends = splits + [ len(string) ]
#    return [string[start:end] for start, end in zip(starts, ends)]


def sentences(text):
    return minisplit(EOS, text)


def lines2sentences(ins):
    todo = ''
    for line in ins:
        ss, todo = sentences(re.sub('\s+', ' ', todo) + line)
        for s in ss:
            yield s
            

            
# this is the only sensible way to frame text without a line-joiner/sentence analyser

def tokenize_chunks(text,
                    window_size,
                    overlap_size,
                    split_regex='\W+',
                    filter=clean,
                    lstrip_bag=unicode_punctuation(),
                    rstrip_bag=unicode_punctuation()):
    
    """Chunking stream white-space tokenizer."""
    
    tokens = [tok for tok in re.split(split_regex, text) if filter(tok)]
    
    chunks = (tokens[a:a+window_size] for a in range(0,len(tokens), window_size-overlap_size))
        
    for n, chunk in enumerate(chunks):
        toks = [unicodedata.normalize('NFKD', t) for t in [token.lstrip(lstrip_bag).rstrip(rstrip_bag) for token in chunk] if filter(t)]
        if len(toks) > 0:
            yield toks


def tokenize(text,
             filter=clean,
             lstrip_bag=unicode_punctuation(),
             rstrip_bag=unicode_punctuation()):
    
    """white-space tokenizer."""
    # filter post stripping...
    return (unicodedata.normalize('NFKD', t)
                for t in (tok.lstrip(lstrip_bag).rstrip(rstrip_bag) for tok in re.split("\s+", text))
                if filter(t))
    
        
def tokenize_filtered_sentences(text, maxlen, minlen, filter, lstrip_bag, rstrip_bag):
    "generate normalized token lists from sentences filtered by length in text"
    for i, sentence in enumerate(sentence_split(text)):
            
        l = len(sentence.split())
            
        if l < minlen or l > maxlen:
            continue
        else:
            frame = tokenize(sentence, filter=filter, lstrip_bag=lstrip_bag, rstrip_bag=rstrip_bag)
            yield (i, frame)


#
# run tokenizer of choice...
#
                
def run(args, data_iter):
    """Get input text from line iterator (or stream) and tokenize"""
    # currently reading one json /doc per line 
    for l in data_iter:

        data = json.loads(l.strip())
        
        if not data.get(args.field, None):
            print(args.field, 'content field not found in:', data, file=sys.stderr)
            continue
        
        else:
            fid = data.get(args.frame, None)
            if fid:
                text = data.get(args.field)
                chunks = tokenize_chunks(text, args.window_size, args.overlap_size,
                                         split_regex=args.split_regex,
                                         filter=args.filter,
                                         lstrip_bag=args.lstrip_bag,
                                         rstrip_bag=args.rstrip_bag)
                for toks in chunks:
                    print('{:s}\t{:s}'.format(fid, '\t'.join(toks)))
            else:
                print(args.frame, 'id field not found in:', data, file=sys.stderr)

            
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Text analysis on stdio')
    parser.add_argument('--window-size', type=int, default=15)
    parser.add_argument('--overlap-size', type=int, default=2)
    #parser.add_argument('--maxlen', type=int, required=True)
    #parser.add_argument('--minlen', type=int, required=True)
    parser.add_argument('--split-regex', type=str, default='\s+')
    parser.add_argument('--filter', type=filter, default=minimal)
    parser.add_argument('--rstrip-bag', type=str, default=unicode_punctuation())
    parser.add_argument('--lstrip-bag', type=str, default=unicode_punctuation())
    parser.add_argument('--field', type=str, required=True)
    parser.add_argument('--frame', type=str, required=True)
    args = parser.parse_args()
    run(args, sys.stdin)

                        
