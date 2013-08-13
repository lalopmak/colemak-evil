License

This document is licensed under the CC0 1.0 Public Domain Declaration, as
released by Creative Commons <http://creativecommons.org/publicdomain/zero/1.0/>.

THIS DOCUMENT IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS",
WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
DOCUMENT, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

---

NB: This is NOT an expert opinion - I am an amateur when it comes to these subjects.  I am mainly writing this down so that I don't forget later.

This document gives some explanations as to why I chose current positions over, say, default colemak.vim/colemak-evil.  Though it has the classic unei movement keys that distinguish this class of mappings, colemak.vim is still overly reliant on mnenomics, leading to questionable key placements like r (replace) and o (open).  I attempt to improve upon these at the further cost of mnemonics, aiming for ergonomicness and geometric suggestiveness (though the latter has been reduced since find-char was made redundant).  


Most obviously, many formerly shifted keys are instead modified with C-.  This is because, on my keyboard (as well as probably most emacs users') C- is bound to caps lock, an easier key to reach.

Navigation keys
----
NI were changed to forward/backward sentence, UE forward/backward paragraph.  The previous 5x bindings were kinda redundant.  There might be better choices for this.

---

Change was moved to t, due to its very common usage, especially comboed with ace-jump (wf).  It is also commonly comboed with rs for inner/outer text objects. The close proximity to delete (d) is coincidential, but enhances suggestiveness.

; (evil-ex) is probably the most common command.  Moving it from default placement is slightly problematic in that it may not be moved in other apps ("legacy issues").  But more importantly, it does not seem to gain from a home-row spot.  All usages of ; end with <RET>, which current positioning of ; (hit by ring finger) helps move ring finger toward.  ; commands also tend to be short so that this gain is not generally neutralized.  For example, ;c<RET> is much more efficient than oc<RET>, because after hitting ; the pinkie is already primed to hit <RET>.

undo moved to p/C-p.  z/Z is viable and has nice shift properties.  It helps that z is not used often (most usage is in bundles) - that is also why p was chosen over, say, v.  However, z/Z requires too much folding of the ring finger which, while strong, could eventually be problematic.  Also, p is a good choice for a consecutive, "nonflowing command", since it is awkward to move from it. 

open above/below are not used nearly often enough to deserve home row spots.  Currently, the spots open are qzx, of which x is out of the question.  z/Z is not a bad spot, especially for such a "nonflowing command", and moves finger toward shift which is somewhat common when opening new line, but it is an open question whether it is preferable over q/C-q, and also whether the relative frequencies justify best placement.

open's old "o" position was replaced by ace-jump-line. Its an improvement over its original p position, which is awkward for a "flowing command", since it requires reaching back the index-finger.  It's also not paired with <RET>, eliminating a possibility of repeated finger, though the repeated possibility does come up in the ace-jumping itself.

substitute currently at x.  Questionable placement, though one advantage is that x works as a cut.  Alternatives include q and o.  Will depend on usage frequencies.

Though I considered leaving b/B (buffer/file) as a ; command, switching buffers is ultimately too common.
