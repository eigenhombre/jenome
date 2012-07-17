# jenome

A very early cut at code for playing around with the human genome.

### Usage:

Download genome code from:

    http://hgdownload.cse.ucsc.edu/downloads.html

Pull this source tree down and run src/jenome/core.clj in the repl:

    $ lein repl
    ; Tell it where you put the genome file:
    (def genome-file
       (atom "/path/to/hg19.2bit"))
    ; Run the decoder
    (hg)

This will (or should) spin through all the available base pairs in all
19 sequences.  It doesn't do anything else yet.  I say "should"
because, though the blocks decode in a consistent fashion, I haven't
been able to cross-check this with results from anywhere else yet.

### License

Copyright (C) 2012 John Jacobsen

Distributed under the Eclipse Public License, the same as Clojure.

### Disclaimer

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT OF THIRD PARTY RIGHTS. IN
NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
OR OTHER DEALINGS IN THE SOFTWARE.

(Basically, use at your own risk.)
