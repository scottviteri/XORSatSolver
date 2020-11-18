<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html><head><meta http-equiv="content-type" content="text/html; charset=utf-8"/><meta name="viewport" content="width=device-width, initial-scale=0.8"/><title>Day 7</title><link rel="stylesheet" type="text/css" href="scribble.css" title="default"/><link rel="stylesheet" type="text/css" href="racket.css" title="default"/><link rel="stylesheet" type="text/css" href="manual-style.css" title="default"/><link rel="stylesheet" type="text/css" href="manual-racket.css" title="default"/><script type="text/javascript" src="scribble-common.js"></script><script type="text/javascript" src="manual-racket.js"></script><!--[if IE 6]><style type="text/css">.SIEHidden { overflow: hidden; }</style><![endif]--></head><body id="scribble-racket-lang-org"><div class="tocset"><div class="tocview"><div class="tocviewlist tocviewlisttopspace"><div class="tocviewtitle"><table cellspacing="0" cellpadding="0"><tr><td style="width: 1em;"><a href="javascript:void(0);" title="Expand/Collapse" class="tocviewtoggle" onclick="TocviewToggle(this,&quot;tocview_0&quot;);">&#9658;</a></td><td></td><td><a href="" class="tocviewselflink" data-pltdoc="x">Day 7</a></td></tr></table></div><div class="tocviewsublistonly" style="display: none;" id="tocview_0"><table cellspacing="0" cellpadding="0"><tr><td align="right">1&nbsp;</td><td><a href="#%28part._.What_s_the_signal_on_wire_a_%29" class="tocviewlink" data-pltdoc="x">What&rsquo;s the signal on wire <span class="stt">a</span>?</a></td></tr><tr><td align="right">2&nbsp;</td><td><a href="#%28part._.What_s_the_signal_on_wire_a_if_wire_b_is_overridden_with_a_s_original_value_%29" class="tocviewlink" data-pltdoc="x">What&rsquo;s the signal on wire <span class="stt">a</span> if wire <span class="stt">b</span> is overridden with <span class="stt">a</span>&rsquo;s original value?</a></td></tr><tr><td align="right">3&nbsp;</td><td><a href="#%28part._.Testing_.Day_7%29" class="tocviewlink" data-pltdoc="x">Testing Day 7</a></td></tr></table></div></div></div><div class="tocsub"><table class="tocsublist" cellspacing="0"><tr><td><span class="tocsublinknumber"></span><a href="#%28part._.Day_7%29" class="tocsubseclink" data-pltdoc="x">Day 7</a></td></tr><tr><td><span class="Smaller"><a href="#%28elem._%28chunk._~3cday07~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07&gt;</a></span></td></tr><tr><td><span class="tocsublinknumber">1<tt>&nbsp;</tt></span><a href="#%28part._.What_s_the_signal_on_wire_a_%29" class="tocsubseclink" data-pltdoc="x">What&rsquo;s the signal on wire <span class="stt">a</span>?</a></td></tr><tr><td><span class="Smaller"><a href="#%28elem._%28chunk._~3cday07-setup~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-setup&gt;</a></span></td></tr><tr><td><span class="Smaller"><a href="#%28elem._%28chunk._~3cday07-ops~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-ops&gt;</a></span></td></tr><tr><td><span class="Smaller"><a href="#%28elem._%28chunk._~3cday07-q1~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-q1&gt;</a></span></td></tr><tr><td><span class="tocsublinknumber">2<tt>&nbsp;</tt></span><a href="#%28part._.What_s_the_signal_on_wire_a_if_wire_b_is_overridden_with_a_s_original_value_%29" class="tocsubseclink" data-pltdoc="x">What&rsquo;s the signal on wire <span class="stt">a</span> if wire <span class="stt">b</span> is overridden with <span class="stt">a</span>&rsquo;s original value?</a></td></tr><tr><td><span class="Smaller"><a href="#%28elem._%28chunk._~3cday07-q2~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-q2&gt;</a></span></td></tr><tr><td><span class="tocsublinknumber">3<tt>&nbsp;</tt></span><a href="#%28part._.Testing_.Day_7%29" class="tocsubseclink" data-pltdoc="x">Testing Day 7</a></td></tr><tr><td><span class="Smaller"><a href="#%28elem._%28chunk._~3cday07-test~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-test&gt;</a></span></td></tr></table></div></div><div class="maincolumn"><div class="main"><div class="versionbox"><span class="versionNoNav">7.8</span></div><h2><a name="(part._.Day_7)"></a><a name="(mod-path._aoc-racket/day07)"></a>Day 7</h2><p><table cellspacing="0" cellpadding="0" class="defmodule"><tr><td align="left" colspan="2"><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">require</span></span></span><span class="stt"> </span><a href="" class="RktModLink" data-pltdoc="x"><span class="RktSym">aoc-racket/day07</span></a><span class="RktPn">)</span></td></tr></table></p><p><a href="http://adventofcode.com/day/7">The puzzle</a>. Our <a href="day07-input.txt">input</a> describes an electrical circuit, with each line of the file describing the signal provided to a particular wire.</p><p><div class="SIntrapara"><a name="(elem._(chunk._~3cday07~3e~3a1))"></a><span style="font-weight: bold"><span style="font-style: italic"><a href="#%28elem._%28chunk._~3cday07~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07&gt;</a></span> ::=</span></div><div class="SIntrapara"><blockquote class="SCodeFlow"><table cellspacing="0" cellpadding="0" class="RktBlk"><tr><td><a href="#%28elem._%28chunk._~3cday07-setup~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-setup&gt;</a></td></tr><tr><td><a href="#%28elem._%28chunk._~3cday07-ops~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-ops&gt;</a></td></tr><tr><td><a href="#%28elem._%28chunk._~3cday07-q1~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-q1&gt;</a></td></tr><tr><td><a href="#%28elem._%28chunk._~3cday07-q2~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-q2&gt;</a></td></tr><tr><td><a href="#%28elem._%28chunk._~3cday07-test~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-test&gt;</a></td></tr></table></blockquote></div></p><h3>1<tt>&nbsp;</tt><a name="(part._.What_s_the_signal_on_wire_a_)"></a><a name="(idx._(gentag._0))"></a><a name="(idx._(gentag._1))"></a><a name="(idx._(gentag._2))"></a><a name="(idx._(gentag._3))"></a>What&rsquo;s the signal on wire <span class="stt">a</span>?</h3><p>The first question we should ask is &#8212; how do we model a wire? We&rsquo;re told that it&rsquo;s a thing with inputs that can be evaluated to get a value. So it sounds a lot like a function. Thus, what we&rsquo;ll do is convert our wire descriptions into functions, and then run the function called <span class="RktSym">a</span>.</p><p>In other languages, creating functions from text strings would be a difficult trick. But this facility is built into Racket with <span class="RktSym"><span class="badlink"><span class="RktValLink">define-syntax</span></span></span>. Essentially our program will run in two phases: in the syntax-transformation phase, we&rsquo;ll read in the list of wire descriptions and expand them into code that represents functions. In the second phase, the program &#8212; including our new functions, created via syntax transformation &#8212; will compile &amp; run as usual.</p><p>The <span class="RktSym">convert-input-to-wire-functions</span> transformer takes the input strings and first converts each into a <span style="font-style: italic">datum</span> &#8212; that is, a fragment of Racket code. So an input string like this:</p><p><span class="RktVal">"bn RSHIFT 2 -&gt; bo"</span></p><p>becomes a datum like this:</p><p><span class="RktPn">(</span><span class="RktSym">wire</span><span class="stt"> </span><span class="RktSym">bn</span><span class="stt"> </span><span class="RktSym">RSHIFT</span><span class="stt"> </span><span class="RktVal">2</span><span class="stt"> </span><span class="RktSym"><span class="badlink"><span class="RktValLink">-&gt;</span></span></span><span class="stt"> </span><span class="RktSym">bo</span><span class="RktPn">)</span></p><p>Next, this transformer converts the datums into <span style="font-style: italic">syntax</span>, a process that adds contextual information (for instance, the meanings of identifiers) so the code can be evaluated.</p><p>Then the <span class="RktSym">wire</span> transformer moves the arguments around to define functions, by matching the three definition patterns that appear in the input. Thus, syntax like this:</p><p><span class="RktPn">(</span><span class="RktSym">wire</span><span class="stt"> </span><span class="RktSym">bn</span><span class="stt"> </span><span class="RktSym">RSHIFT</span><span class="stt"> </span><span class="RktVal">2</span><span class="stt"> </span><span class="RktSym"><span class="badlink"><span class="RktValLink">-&gt;</span></span></span><span class="stt"> </span><span class="RktSym">bo</span><span class="RktPn">)</span></p><p>becomes:</p><p><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="stt"> </span><span class="RktPn">(</span><span class="RktSym">bo</span><span class="RktPn">)</span><span class="stt"> </span><span class="RktPn">(</span><span class="RktSym">RSHIFT</span><span class="stt"> </span><span class="RktPn">(</span><span class="RktSym">evaluate-arg</span><span class="stt"> </span><span class="RktSym">bn</span><span class="RktPn">)</span><span class="stt"> </span><span class="RktPn">(</span><span class="RktSym">evaluate-arg</span><span class="stt"> </span><span class="RktVal">2</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">)</span></p><p><span class="RktSym">evaluate-arg</span> lets us handle the fact that some of the arguments for our wires are other wires, and some arguments are numbers. Rather than detect these differences during the syntax-transformation phase, we&rsquo;ll just wrap every input argument with <span class="RktSym">evaluate-arg</span>, which will do the right thing in the next phase.</p><p>(<span class="RktSym">wire-value-cache</span> is just a performance enhancement, so that wire values don&rsquo;t have to be computed multiple times.)</p><p>One gotcha when using syntax transformers is that identifiers introduced by a transformer can silently override others (in the same way that identifiers defined inside a <span class="RktSym"><span class="badlink"><span class="RktValLink">let</span></span></span> will override those with the same name outside the <span class="RktSym"><span class="badlink"><span class="RktValLink">let</span></span></span>). For instance, one of the wires in our input is named <span class="stt">if</span>. When our syntax transformer defines the <span class="stt">if</span> function, it will override the usual meaning of <span class="RktSym"><span class="badlink"><span class="RktValLink">if</span></span></span>. There are plenty of elegant ways to prevent these name collisions. (The most important of which is called <span style="font-style: italic">syntax hygiene</span>, and permeates the design of Racket&rsquo;s syntax-transformation system.) But because this is a puzzle, we&rsquo;ll take the cheap way out: we won&rsquo;t use <span class="RktSym"><span class="badlink"><span class="RktValLink">if</span></span></span> elsewhere in our code, and instead use <span class="RktSym"><span class="badlink"><span class="RktValLink">cond</span></span></span>.</p><p><div class="SIntrapara"><a name="(elem._(chunk._~3cday07-setup~3e~3a1))"></a><span style="font-weight: bold"><span style="font-style: italic"><a href="#%28elem._%28chunk._~3cday07-setup~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-setup&gt;</a></span> ::=</span></div><div class="SIntrapara"><blockquote class="SCodeFlow"><table cellspacing="0" cellpadding="0" class="RktBlk"><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">require</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">racket</span><span class="hspace">&nbsp;</span><span class="RktSym">rackunit</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">for-syntax</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">racket/file</span><span class="hspace">&nbsp;</span><span class="RktSym">racket/string</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">provide</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">all-defined-out</span></span></span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define-syntax</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">convert-input-to-wire-functions</span><span class="hspace">&nbsp;</span><span class="RktSym">stx</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">syntax-case</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">stx</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">_</span></span></span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">let*</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktPn">[</span><span class="RktSym">input-strings</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">file-&gt;lines</span></span></span><span class="hspace">&nbsp;</span><span class="RktVal">"day07-input.txt"</span><span class="RktPn">)</span><span class="RktPn">]</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktSym">wire-strings</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">map</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">&#955;</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">str</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">format</span></span></span><span class="hspace">&nbsp;</span><span class="RktVal">"(wire ~a)"</span><span class="hspace">&nbsp;</span><span class="RktSym">str</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktSym">input-strings</span><span class="RktPn">)</span><span class="RktPn">]</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktSym">wire-datums</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">map</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">compose1</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym"><span class="badlink"><span class="RktValLink">read</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym"><span class="badlink"><span class="RktValLink">open-input-string</span></span></span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktSym">wire-strings</span><span class="RktPn">)</span><span class="RktPn">]</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">datum-&gt;syntax</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">stx</span><span class="hspace">&nbsp;</span><span class="RktVal">`</span><span class="RktVal">(</span><span class="RktVal">begin</span><span class="hspace">&nbsp;</span><span class="RktRdr">,@</span><span class="RktSym">wire-datums</span><span class="RktVal">)</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">]</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define-syntax</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">wire</span><span class="hspace">&nbsp;</span><span class="RktSym">stx</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">syntax-case</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">stx</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">-&gt;</span></span></span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">_</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">arg</span><span class="hspace">&nbsp;</span><span class="RktSym"><span class="badlink"><span class="RktValLink">-&gt;</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">wire-name</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktRdr">#'</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">wire-name</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">evaluate-arg</span><span class="hspace">&nbsp;</span><span class="RktSym">arg</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">]</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">_</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">16bit-op</span><span class="hspace">&nbsp;</span><span class="RktSym">arg</span><span class="hspace">&nbsp;</span><span class="RktSym"><span class="badlink"><span class="RktValLink">-&gt;</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">wire-name</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktRdr">#'</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">wire-name</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">16bit-op</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">evaluate-arg</span><span class="hspace">&nbsp;</span><span class="RktSym">arg</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">]</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">_</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">arg1</span><span class="hspace">&nbsp;</span><span class="RktSym">16bit-op</span><span class="hspace">&nbsp;</span><span class="RktSym">arg2</span><span class="hspace">&nbsp;</span><span class="RktSym"><span class="badlink"><span class="RktValLink">-&gt;</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">wire-name</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktRdr">#'</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">wire-name</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">16bit-op</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">evaluate-arg</span><span class="hspace">&nbsp;</span><span class="RktSym">arg1</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">evaluate-arg</span><span class="hspace">&nbsp;</span><span class="RktSym">arg2</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">]</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">_</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">expr</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktRdr">#'</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">begin</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">expr</span><span class="RktPn">)</span><span class="RktPn">]</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktSym"><span class="badlink"><span class="RktValLink">else</span></span></span><span class="hspace">&nbsp;</span><span class="RktRdr">#'</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">void</span></span></span><span class="RktPn">)</span><span class="RktPn">]</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym">convert-input-to-wire-functions</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">wire-value-cache</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">make-hash</span></span></span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">evaluate-arg</span><span class="hspace">&nbsp;</span><span class="RktSym">x</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">cond</span></span></span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">procedure?</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">x</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">hash-ref!</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">wire-value-cache</span><span class="hspace">&nbsp;</span><span class="RktSym">x</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">thunk*</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">x</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">]</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktSym"><span class="badlink"><span class="RktValLink">else</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">x</span><span class="RktPn">]</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr></table></blockquote></div></p><p>We also need to implement our 16-bit math operations. As we saw above, our syntax transformers are generating code that looks like, for instance, <span class="RktPn">(</span><span class="RktSym">RSHIFT</span><span class="stt"> </span><span class="RktPn">(</span><span class="RktSym">evaluate-arg</span><span class="stt"> </span><span class="RktSym">bn</span><span class="RktPn">)</span><span class="stt"> </span><span class="RktPn">(</span><span class="RktSym">evaluate-arg</span><span class="stt"> </span><span class="RktVal">2</span><span class="RktPn">)</span><span class="RktPn">)</span>. This code won&rsquo;t work unless we&rsquo;ve defined an <span class="RktSym">RSHIFT</span> function too.</p><p>These next definitions use <span class="RktSym"><span class="badlink"><span class="RktValLink">define-syntax-rule</span></span></span> as a shortcut, which is another syntax transformer. (Thanks to <a href="https://jeapostrophe.github.io">Jay McCarthy</a> for the 16-bit operations.)</p><p><div class="SIntrapara"><a name="(elem._(chunk._~3cday07-ops~3e~3a1))"></a><span style="font-weight: bold"><span style="font-style: italic"><a href="#%28elem._%28chunk._~3cday07-ops~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-ops&gt;</a></span> ::=</span></div><div class="SIntrapara"><blockquote class="SCodeFlow"><table cellspacing="0" cellpadding="0" class="RktBlk"><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">16bitize</span><span class="hspace">&nbsp;</span><span class="RktSym">x</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">16bit-max</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">expt</span></span></span><span class="hspace">&nbsp;</span><span class="RktVal">2</span><span class="hspace">&nbsp;</span><span class="RktVal">16</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">r</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">modulo</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">x</span><span class="hspace">&nbsp;</span><span class="RktSym">16bit-max</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">cond</span></span></span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">negative?</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">r</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">16bitize</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">+</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">16bit-max</span><span class="hspace">&nbsp;</span><span class="RktSym">r</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">]</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;&nbsp;&nbsp;</span><span class="RktPn">[</span><span class="RktSym"><span class="badlink"><span class="RktValLink">else</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">r</span><span class="RktPn">]</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define-syntax-rule</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">define-16bit</span><span class="hspace">&nbsp;</span><span class="RktSym">id</span><span class="hspace">&nbsp;</span><span class="RktSym">proc</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">id</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">compose1</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">16bitize</span><span class="hspace">&nbsp;</span><span class="RktSym">proc</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym">define-16bit</span><span class="hspace">&nbsp;</span><span class="RktSym">AND</span><span class="hspace">&nbsp;</span><span class="RktSym"><span class="badlink"><span class="RktValLink">bitwise-and</span></span></span><span class="RktPn">)</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym">define-16bit</span><span class="hspace">&nbsp;</span><span class="RktSym">OR</span><span class="hspace">&nbsp;</span><span class="RktSym"><span class="badlink"><span class="RktValLink">bitwise-ior</span></span></span><span class="RktPn">)</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym">define-16bit</span><span class="hspace">&nbsp;</span><span class="RktSym">LSHIFT</span><span class="hspace">&nbsp;</span><span class="RktSym"><span class="badlink"><span class="RktValLink">arithmetic-shift</span></span></span><span class="RktPn">)</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym">define-16bit</span><span class="hspace">&nbsp;</span><span class="RktSym">RSHIFT</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">&#955;</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">x</span><span class="hspace">&nbsp;</span><span class="RktSym">y</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">arithmetic-shift</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">x</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">-</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">y</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym">define-16bit</span><span class="hspace">&nbsp;</span><span class="RktSym">NOT</span><span class="hspace">&nbsp;</span><span class="RktSym"><span class="badlink"><span class="RktValLink">bitwise-not</span></span></span><span class="RktPn">)</span></td></tr></table></blockquote></div></p><p>After that, we just evaluate wire function <span class="RktSym">a</span> to get our answer.</p><p><div class="SIntrapara"><a name="(elem._(chunk._~3cday07-q1~3e~3a1))"></a><span style="font-weight: bold"><span style="font-style: italic"><a href="#%28elem._%28chunk._~3cday07-q1~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-q1&gt;</a></span> ::=</span></div><div class="SIntrapara"><blockquote class="SCodeFlow"><p><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">q1</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">a</span><span class="RktPn">)</span><span class="RktPn">)</span></p></blockquote></div></p><h3>2<tt>&nbsp;</tt><a name="(part._.What_s_the_signal_on_wire_a_if_wire_b_is_overridden_with_a_s_original_value_)"></a><a name="(idx._(gentag._4))"></a>What&rsquo;s the signal on wire <span class="stt">a</span> if wire <span class="stt">b</span> is overridden with <span class="stt">a</span>&rsquo;s original value?</h3><p>Having done the heavy lifting, this is easy. We&rsquo;ll redefine wire function <span class="RktSym">b</span> to produce the new value, and then check the value of <span class="RktSym">a</span> again.</p><p>Ordinarily, as a safety measure, Racket won&rsquo;t let you redefine functions. But we can circumvent this limitation by setting <span class="RktSym"><span class="badlink"><span class="RktValLink">compile-enforce-module-constants</span></span></span> to <span class="RktVal">#f</span>. We&rsquo;ll also need to reset our cache, since this change will affect the other wires too.</p><p><div class="SIntrapara"><a name="(elem._(chunk._~3cday07-q2~3e~3a1))"></a><span style="font-weight: bold"><span style="font-style: italic"><a href="#%28elem._%28chunk._~3cday07-q2~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-q2&gt;</a></span> ::=</span></div><div class="SIntrapara"><blockquote class="SCodeFlow"><table cellspacing="0" cellpadding="0" class="RktBlk"><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">compile-enforce-module-constants</span></span></span><span class="hspace">&nbsp;</span><span class="RktVal">#f</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;</span></td></tr><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">q2</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">define</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">first-a-val</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">a</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">set!</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">b</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">thunk*</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">first-a-val</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">set!</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">wire-value-cache</span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">make-hash</span></span></span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym">a</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr></table></blockquote></div></p><h3>3<tt>&nbsp;</tt><a name="(part._.Testing_.Day_7)"></a>Testing Day 7</h3><p><div class="SIntrapara"><a name="(elem._(chunk._~3cday07-test~3e~3a1))"></a><span style="font-weight: bold"><span style="font-style: italic"><a href="#%28elem._%28chunk._~3cday07-test~3e~3a1%29%29" class="plainlink" data-pltdoc="x">&lt;day07-test&gt;</a></span> ::=</span></div><div class="SIntrapara"><blockquote class="SCodeFlow"><table cellspacing="0" cellpadding="0" class="RktBlk"><tr><td><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">module+</span></span></span><span class="hspace">&nbsp;</span><span class="RktSym">test</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">check-equal?</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">q1</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktVal">46065</span><span class="RktPn">)</span></td></tr><tr><td><span class="hspace">&nbsp;&nbsp;</span><span class="RktPn">(</span><span class="RktSym"><span class="badlink"><span class="RktValLink">check-equal?</span></span></span><span class="hspace">&nbsp;</span><span class="RktPn">(</span><span class="RktSym">q2</span><span class="RktPn">)</span><span class="hspace">&nbsp;</span><span class="RktVal">14134</span><span class="RktPn">)</span><span class="RktPn">)</span></td></tr></table></blockquote></div></p></div></div><div id="contextindicator">&nbsp;</div></body></html>