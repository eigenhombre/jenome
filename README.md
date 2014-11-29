# jenome

*"DNA is source code for the most complex machine in the universe."*
--Randall Munroe, "What If?"

Code for playing around with genome data, human or otherwise.


### Testing

    lein midje # add --lazytest to automatically retest after updating code

### Process a .2bit genome file (convert to FASTA-like plain text in stdout):

First, download some `.2bit` genome code from e.g.
http://hgdownload.cse.ucsc.edu/downloads.html. Or, use
`resources/sacCer3.2bit`.

Then,

    lein run <filename.2bit>

See also the commented-out example expressions at the bottom of `src/jenome/core.clj`.

### License

Copyright (C) 2012-2013 John Jacobsen

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
