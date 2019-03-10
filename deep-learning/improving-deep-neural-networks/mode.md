<script type="text/javascript"
   src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>

### dev and test set should:same distribution

- basic 偏差 改进算法
   + (use big network)
- variance : 方差(overfill) 方法 regularizing
   + get more data
   + **L2 regularization**
       $$ j = \sum_{i=1}^m loss(y,\hat{y}) + \sum\sum W$$
   + dropout
       random set(keep_prob) neural to zero
   + weighted averages

- optimization
  + **normalizing** : 将输入的feature映射到一个固定大小的区间e.i(0-1)

-  optimization algorithms
  + mini batch 
  + momentum (exponentially weighted averages)
    * $$ v_w = \beta_1v_w +(1-\beta_1)dw $$
    * $$ W = W +  av_w$$
    * RMSprop
    * **Adam optimnization**
      $$ v_w = \beta_1v_w +(1-\beta_1)dw $$
      $$ v_w^{corrected} = \frac{v_w}{1-\beta_1^t} $$
      $$ s_w = \beta_2s_w+(1-\beta_2)(dw^2)$$
      $$ s_w^{corrected} = \frac{s_w}{1-\beta_2^t}$$
      $$ w = w - a\frac{v_w^{corrected}}{\sqrt{s_w^corrected}} $$

---

  * Vanishing:
      when 神经网络is deep ; Y= w1*w2*w2...wm * X ;
      if (w1 is <1) then Y -> 0
  * exploding:
      if (w1 is >1) then Y -> oo

### solution : batch normalization
    $$ gama= Var[X] $$
    $$ \beta = E[X] $$
    $$ X = \frac{(X - \beta)}{\sqrt{gama+ e}} $$










