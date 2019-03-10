<script type="text/javascript"
   src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>

### p
$$ P(x|y)=\frac{P(y|x)P(x)}{P(y)}$$

### entropy
对单个事件\\(x\\):
$$ h(x) = -log_2(P(x))$$

对随机变量\\(X\\):
$$ H(X)=-\sum_{x}P(x)log(P(x))$$

联合熵:
$$ H(X,Y)=-\sum_x{\sum_y{P(x,y)*log(P(x,y))}}$$

若 \\(X,Y\\)独立:
$$H(X,Y)=H(X) + H(Y)$$

相对熵:
$$D(P||Q)=\sum_x{P(x)*log(\frac{P(x)}{Q(x)})}$$

cibbs不等式:
$$D(P||Q)>=0$$

凸:
$$ f(\lambda x_1+(1-\lambda)x_2) <=
   \lambda f(x_1) + (1-\lambda)f(x_2)
$$

jensen不等式:
$$E[f(x)] >= f(E[X])$$

$$
H_2(P)
=H(p,1-p)
=p*log(1/p)+(1-p)*log(\frac{1}{1-p})
$$

香农信息编码定理:<br>
- 随机变量X,
- H=H(X)
- N 对于由随机变量X组成的序列的长度
- \\(\sigma\\)差错
- \\(H_{\sigma}(X^N)\\)使用差错编码后随机变量 \\(X^N\\)的熵
$$
|\frac{1}{N}H_{\sigma}(X^N)-H| <\epsilon
$$
当N趋近于无穷时,entropy 为\\(H(X)\\)的N个独立的随机变量分布,
可以被压缩成多余\\(NH(X)\\)比特,使得信息丢失的风险趋近于0


### 压缩
前缀码:如果没有码字是其他任何码字的前缀
- 总体X: 随机事件X, 的集合
- 符号表C: 对事件x的映射(编码)
- l(x): 对事件x使用符号表映射后的长度

对于总体X,符号表C,的期望长度L(C,X):
$$L(C,X)=\sum_x P(x)*l(x)$$

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
