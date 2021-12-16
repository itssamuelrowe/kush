/*
 * Copyright 2017-2020 Samuel Rowe, Joel E. Rego
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <jtk/collection/array/Arrays.h>
#include <kush/token-stream.h>

static void initialize(TokenStream* stream);

TokenStream* tokenStreamNew(Compiler* compiler,
                                   Lexer* lexer, TokenChannel channel) {
    jtk_Assert_assertObject(lexer, "The specified lexer is null.");

    TokenStream* stream = allocate(TokenStream, 1);
    stream->compiler = compiler;
    stream->lexer = lexer;
    stream->tokens = jtk_ArrayList_newWithCapacity(128);
    stream->p = -1;
    stream->hitEndOfStream = false;
    stream->channel = channel;
    stream->trash = jtk_ArrayList_new();

    return stream;
}

void destroyStaleTokens(TokenStream* stream) {
}

// TODO: The tokens must be destroyed!
void tokenStreamDelete(TokenStream* stream) {
    jtk_Assert_assertObject(stream, "The specified token stream is null.");

    jtk_ArrayList_delete(stream->tokens);
    deallocate(stream);
}

void resetTokenStream(TokenStream* stream) {
    jtk_Assert_assertObject(stream, "The specified token stream is null.");

    jtk_ArrayList_clear(stream->tokens);
    stream->p = -1;
    stream->hitEndOfStream = false;
}

int32_t k_TokenStream_getIndex(TokenStream* stream) {
    jtk_Assert_assertObject(stream, "The specified token stream is null.");
    return stream->p;
}

int32_t k_TokenStream_getSize(TokenStream* stream) {
    jtk_Assert_assertObject(stream, "The specified token stream is null.");
    return jtk_ArrayList_getSize(stream->tokens);
}

void consumeToken(TokenStream* stream) {
    bool skip;
    if (stream->p >= 0) {
        if (stream->hitEndOfStream) {
            /* The end-of-stream token can be consumed only once.
             * Should we check this condition?
             */
            skip = stream->p < (jtk_ArrayList_getSize(stream->tokens) - 1);
        }
        else {
            /* Have we consumed all the buffered tokens? */
            skip = stream->p < jtk_ArrayList_getSize(stream->tokens);
        }
    }
    else {
        /* Currently, the token stream has no tokens to consume. */
        skip = false;
    }

    jtk_Assert_assertFalse((!skip && (k_TokenStream_la(stream, 1) == TOKEN_END_OF_STREAM)), "...");

    bool hasToken = synchronizeTokens(stream, stream->p + 1);
    if (hasToken) {
        stream->p = getNextTokenOnChannel(stream, stream->p + 1, stream->channel);
    }
}

bool synchronizeTokens(TokenStream* stream, int32_t i) {
    jtk_Assert_assertObject(stream, "The specified token source is null.");
    jtk_Assert_assertTrue(i >= 0, "The specified index is invalid.");

    int32_t n = i - jtk_ArrayList_getSize(stream->tokens) + 1;
    bool result = true;
    if (n > 0) {
        int32_t fetched = fetchTokens(stream, n);
        result = fetched >= n;
    }
    return result;
}

int32_t fetchTokens(TokenStream* stream, int32_t n) {
    jtk_Assert_assertObject(stream, "The specified token source is null.");

    if (stream->hitEndOfStream) {
        return 0;
    }

    int32_t oldSize = jtk_ArrayList_getSize(stream->tokens);
    int32_t i;
    for (i = 0; i < n; i++) {
        Token* token = nextToken(stream->lexer);
        token->index = oldSize + i;
        jtk_ArrayList_add(stream->tokens, token);
        jtk_ArrayList_add(stream->trash, token);

        if (token->type == TOKEN_END_OF_STREAM) {
            stream->hitEndOfStream = true;
            return i + 1;
        }
    }
    return n;
}

Token* getToken(TokenStream* stream, int32_t index) {
    jtk_Assert_assertObject(stream, "The specified token source is null.");

    /* Index-out-of-range errors are checked by the
     * jtk_ArrayList_t class.
     */
    return jtk_ArrayList_getValue(stream->tokens, index);
}

jtk_ArrayList_t* getTokens(TokenStream* stream,
        int32_t startIndex, int32_t stopIndex) {
    jtk_Assert_assertObject(stream, "The specified token source is null.");

    int32_t size = jtk_ArrayList_getSize(stream->tokens);
    jtk_Arrays_checkRange(size, startIndex, stopIndex);

    initialize(stream);
    jtk_ArrayList_t* result = jtk_ArrayList_new();
    int32_t i;
    for (i = startIndex; i < stopIndex; i++) {
        Token* token = (Token*)jtk_ArrayList_getValue(stream->tokens, i);
        jtk_ArrayList_add(result, token);
    }
    /* The user is responsible for the destruction of
     * this list.
     */
    return result;
}

TokenType k_TokenStream_la(TokenStream* stream, int32_t i) {
    return k_TokenStream_lt(stream, i)->type;
}

Token* k_TokenStream_lt(TokenStream* stream, int32_t k) {
    jtk_Assert_assertObject(stream, "The specified token source is null.");

    initialize(stream);
    Token* token = NULL;
    if (k != 0) {
        if (k < 0) {
            if ((stream->p - k) >= 0) {
                int32_t i = stream->p;
                int32_t n = 1;
                while ((n <= k) && (i > 0)) {
                    i = k_TokenStream_getPreviousTokenOnChannel(stream, i - 1, stream->channel);
                    n++;
                }
                if (i >= 0) {
                    token = (Token*)jtk_ArrayList_getValue(stream->tokens, i);
                }
            }
        }
        else {
            int32_t i = stream->p;
            int32_t n = 1;
            while (n < k) {
                bool hasToken = synchronizeTokens(stream, i + 1);
                if (hasToken) {
                    i = getNextTokenOnChannel(stream, i + 1, stream->channel);
                }
                n++;
            }
            token = (Token*)jtk_ArrayList_getValue(stream->tokens, i);
        }
    }
    return token;
}

void initialize(TokenStream* stream) {
    jtk_Assert_assertObject(stream, "The specified token source is null.");

    if (stream->p == -1) {
        synchronizeTokens(stream, 0);
        stream->p = getNextTokenOnChannel(stream, 0, stream->channel);
    }
}

int32_t k_TokenStream_getPreviousTokenOnChannel(TokenStream* stream,
        int32_t i, TokenChannel channel) {
    jtk_Assert_assertObject(stream, "The specified token source is null.");

    /* Ensure that the token stream has buffered, at least,
     * tokens till the requested index.
     */
    synchronizeTokens(stream, i);
    int32_t size = jtk_ArrayList_getSize(stream->tokens);
    if (i >= size) {
        /* In case the synchronization failed to retrieve the
         * requested number of tokens, return the index of the
         * end-of-stream token.
         */
        return size - 1;
    }

    while (i >= 0) {
        Token* token = (Token*)jtk_ArrayList_getValue(stream->tokens, i);
        if ((token->type == TOKEN_END_OF_STREAM) || (token->channel == channel)) {
            return i;
        }
        i--;
    }

    return -1;
}

int32_t getNextTokenOnChannel(TokenStream* stream,
        int32_t i, TokenChannel channel) {
    jtk_Assert_assertObject(stream, "The specified token source is null.");

    /* Ensure that the token stream has buffered, at least,
     * tokens till the requested index.
     */
    synchronizeTokens(stream, i);
    int32_t size = jtk_ArrayList_getSize(stream->tokens);
    if (i >= size) {
        /* In case the synchronization failed to retrieve the
         * requested number of tokens, return the index of the
         * end-of-stream token.
         */
        return size - 1;
    }

    Token* token = jtk_ArrayList_getValue(stream->tokens, i);
    while (token->channel != channel) {
        /* In case the token stream has reached the end-of-stream,
         * return the index of the end-of-stream token.
         */
        if (token->type == TOKEN_END_OF_STREAM) {
            return i;
        }

        /* The token source failed to find a token on the
         * requested channel at this index. Increment it
         * to the next index.
         */
        i++;
        /* Ensure that the token stream has buffered, at least,
         * tokens till the new index.
         */
        synchronizeTokens(stream, i);
        /* Update the token variable for next iteration. */
        token = (Token*)jtk_ArrayList_getValue(stream->tokens, i);
    }
    return i;
}

void fillTokenStream(TokenStream* stream) {
    initialize(stream);
    /* The token stream tries to buffer a 1000 tokens
     * at each iteration. This is repeated until the token
     * stream fails to fetch the quota, which indicates
     * complete tokenization of the input stream.
     */
    int32_t blockSize = 1000;
    int32_t fetched;
    do {
        fetched = fetchTokens(stream, blockSize);
    } while (fetched == blockSize);
}

int32_t k_TokenStream_getNumberOfTokens(TokenStream* stream,
                                        TokenChannel channel) {
    jtk_Assert_assertObject(stream, "The specified token source is null.");

    /* Fetch all the tokens from the input stream before computing
     * the number of tokens on the specified channel.
     */
    fillTokenStream(stream);

    int32_t n = 0;
    int32_t size = jtk_ArrayList_getSize(stream->tokens);
    int32_t i;
    for (i = 0; i < size; i++) {
        Token* token = (Token*)jtk_ArrayList_getValue(stream->tokens, i);
        if (token->channel == channel) {
            n++;
        }
    }
    return n;
}
