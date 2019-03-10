<script type="text/javascript"
   src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>


# neural-networks-deep-leaming

$$x=\frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$$

$$ E_0 = mc^2 $$


- \\(x_ n \\) : size of x
- \\(m \\) : size of train 
- sigma:

### logistic regression: 
$$ sigma(z)=\frac{1}{1+e^(-z)}$$
$ $ z = w^Tx + b $$
$$ a=\hat{y} = sigma(z) $$

- loss function :
$$ loss(a,y)= - ( y*log(a) + (1-y)*log(1-a))$$

- coss function :
$$ J = coss(\hat{y},y) = \frac{\sum_{i=1}^m loss(\hat{y},y)}{m}$$
