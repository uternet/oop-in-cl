# 9 创建并初始化实例

客户端程序通常使用构造函数来创建实例。构造函数调用 `make-instance`，它创建、初始化并返回一个新实例。CLOS使您能够控制初始化的许多方面，从为槽提供默认值到通过编写方法自定义初始化。本章首先描述了 `make-instance` 的参数，并总结了调用 `make-instance` 时CLOS执行的步骤。然后它展示了如何使用这些技术来控制初始化。

## 9.1 创建实例的参数

`make-instance`的语法是：

    make-instance class &rest initargs
    
第一个参数是类，可以是类的名字，也可以是类对象本身。这为您提供了一个线索，即 `make-instance` 跨越了宏和函数之间的边界。事实上，`make-instance` 是应用程序程序员和元对象程序员都使用的强大工具。我们只描述那些适用于应用程序程序员的技术。

`&rest` 参数由initargs(初始化参数)组成，它是 *initialization arguments* 的缩写。初始化参数控制着初始化的某些方面； 它可能会填充一个槽，或者被初始化方法使用，或者两者兼而有之。

每个初始化参数由一个名字和一个值组成。初始化参数的名字可以是任何符号，不一定是关键字。`&rest` 参数的格式与作为 `&key` 参数处理的关键字参数的格式相同。

## 9.2 make-instance的功能摘要

当你调用了`make-instance`，CLOS会执行以下步骤：

1. 将您提供给 `make-instance` 的初始化参数与您没有提交的的所有初始化参数的默认值相结合。结果是一个默认的初始化参数列表。

2. 确保默认初始化参数列表中的所有参数名都有效，如果不是的话，则抛出错误信号。如果`make-instance`的 `:allow-other-keys`参数的值是 T，则所有初始化参数的名字都是有效的。

3. 为实例分配存储空间并创建一个所有槽全部未绑定的实例。

4. 将 `initialize-instance` 泛型函数应用于新创建的实例和默认的初始化参数列表。`initialize-instance`的默认 primary 方法执行以下操作：

  a. 根据默认的初始化列表初始化槽
  b. 初始化所有具有 `:uniforms` 并且仍然未绑定的槽
  
  `initialize-instance` 的 primary 方法通过调用 `shared-initialize` 来完成这项工作，我们在第165页的[程序定义：初始化]()中对此进行了讨论。
  
5. 返回已经完成初始化的实例。

## 9.3 使用defclass选项控制初始化

在这个例子中，我们定义了一些类来表示窗口。这些类定义使用初始化窗口的技术。基本类`window`旨在成为所有窗口的基础。`defclass` 形式使用两个与初始化有关的槽选项—— `:initarg` 和 `:initform` 选项。

```lisp
(defclass window ()
  ((x :initarg :x-position :accessor x-position)
   (y :initarg :y-position :accessor y-position)
   (height :initarg :height :accessor window-height)
   (width :initarg :width :accessor window-width)
   (exposed-p :initform nil :accessor exposed-p))
  (:documentation "Foundation of all windows."))
```

**使用槽选项 :initarg**

在`window`类的定义中，四个槽（x、y、height 和 width）使用了 `:initarg` 选项。将四个符号声明为`window`类有效的初始化参数名字。例如，符号 `:x-position` 是一个初始化参数的名字。如果您将其传递给 `make-instance` ，后面跟一个值，则该值将存储在 `x` 槽中。类似地，符号 `:height` 也是一个初始化参数，可用于初始化名为 `height` 的槽。

```lisp
(make-instance 'window
               :x-position 0
               :y-position 0
               :height 200
               :width 75)
```

请注意，名为`exposed-p` 的槽不使用 `:initarg` 选项。因此，您不能通过为 `make-instance` 传递参数来初始化该槽。并不打算让用户初始化`exposed-p`槽。

**使用槽选项 :initform**

名为`exposed-p` 的槽使用`:initform` 槽选项将默认初始值与槽相关联，该初始值为`nil`。这个槽的语义很简单：当你第一次创建一个窗口时，它是不可见的。我们故意不提供初始化参数，因为我们希望所有新创建的窗口都不可见； 这种方法会导致槽自动初始化为其 `initform`（即 `nil`），有效地预先阻止用户初始化 `exposed-p`槽。

**使用类选项 :default-initargs**

有时，为类提供默认的初始值很有用。`:default-initargs` 类选项执行此操作。它主要用于远程默认； 也就是说，为继承的初始化参数提供默认值。

如果在调用 `make-instance` 时提供了初始化参数，它将覆盖默认的初始化参数。但是如果省略了初始化参数，则使用默认的初始化参数的值。

在定义 window 程序中常用的类时，我们将使用 `:default-initargs`。全屏窗口是覆盖整个屏幕的窗口。这种窗口的高度和宽度是从存储屏幕尺寸的变量中获得的。全屏窗口通常位于原点，因此我们还为 `:x-position` 和 `:y-position` 提供了默认值。

```lisp
(defclass full-screen-window (window) ()
  (:default-initargs
   :x-position *screen-origin-x*
   :y-position *screen-origin-y*
   :height *screen-height*
   :width *screen-width*))
```

这个类可以单独使用，也可以作为其他类的构建块。它与 `window` 具有相同的槽，但它提供了四个initargs的默认值，以方便需要制作全屏窗口的客户端。

**两种默认值**

记住 `:default-initargs` 和 `:initform` 之间的区别很重要。`:default-initargs` 选项为initarg提供默认值，而 `:initform` 选项为槽提供默认值。

如果您打算允许用户初始化槽，那么您应该:

- 使用 `:initarg` 声明一个用于初始化槽的符
- 如果要为该 `:initarg` 提供默认值，请使用 `:default-initargs`

如果您不打算让用户初始化槽，那么您应该

- 不使用 `:initarg` 选项
- 使用`:initform`，如果你想给槽一个默认的初始值

这两个选项如果一起使用就会发生冲突。思考一下当一个槽通过 `:initform` 有了一个默认值并且通过 `:initarg` 又有了一个初始化参数时会发生什么，它本身通过 `:default-initargs` 有一个默认值。`:default-initargs` 中给出的默认值有效地覆盖了 `:initform` 给出的默认值。

对于这两个选项，每次使用时都会求值默认值形式。`:initform` 的值在每次用于初始化槽时都会被求值。每次调用 `make-instance` 时都会求值 `：default-initargs` 中的initarg的值，并且该initarg不会作为 `make-instance` 的参数给出。

## 9.4 用方法控制初始化

调用 `make-instance` 时，它会创建一个实例并调用 `initialize-instance` 泛型函数来初始化新实例。CLOS为 `initialize-instance` 提供了一个默认的 primary 方法，它根据它们的 `initargs` 和 `initforms` 用值来填充槽。您可以通过编写一个用于 `initialize-instance` 的方法来自定义实例的初始化来做额外的工作。

**为`initialize-instance`定义after方法**

一个窗口系统可能需要跟踪所有窗口。在这里，我们将新窗口添加到跟踪已显示窗口的数据结构中去。

```lisp
(defmethod initialize-instance :after ((w window) &key)
  (push w *deexposed-windows*))
```

通常你应该为初始化实例定义 after 方法（就像我们在这里所做的那样）而不是 primary 方法。 primary 方法将覆盖默认的 primary 方法并阻止通常的槽初始化。

由于 `initialize-instance` 的方法接收所有默认的 `initargs` 作为参数，因此 `initialize-instance` 的方法应该在它们的 lambda-list 中使用 `&key`。在这里使用 `&key` 的结果是该方法允许使用关键字而不指定它使用任何关键字参数。有关方法的 lambda-lists 的更多详细信息，请参阅第 132 页的[Congruent Lambda-Lists]()。

**`initialize-instance的默认方法**

`make-instance` 使用实例和默认的 `initarg` 列表调用 `initialize-instance`。使用这些参数，`initialize-instance` 的默认方法用值填充槽，如下所示： 

第一步：如果您向 `make-instance` 提供一个 initarg，那么它的值将存储在关联的槽中。（用来填充槽的initarg由 `:initarg` 槽选项指定。）

第二步：如果槽在步骤一中没有填充，并且initarg具有默认值形式，则求值该形式并将结果存储在槽中。（initarg 的默认值由类选项 `:default-initargs` 指定。）

第三步：如果第二步未填充槽，并且槽具有默认初始值形式，则求值该形式并将结果存储在槽中。（槽的默认值由 槽选项`:initform` 指定。）

第四步：如果第三步也没有填充槽，则槽保持未绑定。

`initialize-instance` 的默认方法通过调用 `shared-initialize` 来完成此初始化工作，`shared-initialize` 是在其他上下文以及创建新实例时调用的泛型函数。我们在第 167 页的[隔离过程之间共享的工作]()中详细描述了 `shared-initialize`。

## 9.5 初始化参数

本节更详细地描述 initargs，重点介绍如何使用它们来初始化新实例。initargs 也用于其他情况； 请参见[通过initargs执行初始化]()，第 168 页。

**初始化参数名字的有效性**

在调用 `make-instance` 之前，必须将initarg名字声明为对给定类有效。有两种方法可以声明initarg名字有效：

1. 槽选项`:initarg`

将符号声明为有效的initarg名字并指定initarg的值所应该存储的槽。这样的符号称为槽填充 initarg。`:initarg` 槽选项由 union 继承：类或其任何超类声明的所有initarg名字对该类都有效。

2. `initialize-instance`方法

将 lambda-list 中的所有`&key`参数名字声明为类的有效initarg名字。(其他泛型函数的方法也将`&key`参数声明为有效的initarg名字。参见[声明initarg名字为有效]()，第170页。)

此外，initarg 名字 `:allow-other-keys` 对所有类都有效。它的默认值为 `nil`，这意味着CLOS检查所有initargs的有效性，如果检测到无效的initarg名字，则会发出错误信号。如果您调用 `make-instance` 并给出 `:allow-other-keys` 后跟非`nil`值，则禁用此错误检查。

如果初始化方法的 lambda-list 使用 `&allow-other-keys`，则所有可能的符号都被声明为有效的 initargs。换句话说，初始化方法中的 `&allow-other-keys` 禁用了对initarg名字的错误检查。

**initarg和槽名字的分离**

当您使用 `:initarg` 槽选项时，initarg 的名字独立于槽的名字。这种独立性允许一定程度的抽象； 客户端不能假设initarg直接映射到同名的槽。事实上，一些initargs可能根本不映射到一个槽，并且一些 槽可能填充了基于几个initargs计算的值。

例如，`triangle`类可能接受三个 initargs，对应每一条边的长度。然而，该类可能被实现为存储两条边的长度和它们之间的角度，而角度可以从三个initargs中计算出来。第三条边的长度将用于计算对角，然后被丢弃，对于客户端来说内部细节是不可见的。

以下示例使用了此方法。请注意，初始化方法将 `:side-a`、`:side-b` 和 `:side-c` 声明为有效的initarg名字，而 `defclass` 形式没有声明任何的initarg名字。我们定义构造函数 `make-triangle` 来创建一个用于创建三角形的抽象接口，并使所有边都成为必需的参数。所有的初始化工作都在`initialize-instance`方法中完成。

```lisp
(defclass triangle (shape)
  ((a :reader side-a)
   (b :reader side-b)
   (angle-C :reader angle-C)))

;;; 在此方法中进行所有初始化
(defmethod initialize-instance :after
  ((tri triangle) &key side-a side-b side-c)
  (let* ((float-a (coerce a 'float))
         (float-b (coerce b 'float))
         (float-c (coerce c 'float))
         (float-angle-C (three-sides-to-angle
                         float-c float-a float-b)))
    (with-slots (a b angle-C) tri
      (setf a float-a)
      (setf b float-b)
      (setf angle-C float-angle-C))))

;;; Define the constructor
(defun make-triangle (side-a side-b side-c)
  (make-instance 'triangle
                 :side-a side-a
                 :side-b side-b
                 :side-c side-c))
```

## 9.6 构造器

我们建议使用构造函数作为创建实例的外部接口，因为构造函数在客户端和实现之间添加了一个有价值的抽象级别。考虑三角形：构造函数的名字 `make-triangle` 意味着“制作三角形”，这是一个比“制作三角形类的实例”更高级的概念。

构造函数的另一个优点是它们可以使用 Common Lisp 参数处理的全部能力。`make-instance` 语法非常有限：在第一个参数（类）之后是一个由initargs组成的 `&rest` 参数。在许多情况下，一个类的语义可以用必需参数、可选参数等更好地表达。例如，对于三角形，`make-instance` 的 `&rest` 参数并不能暗示所有三个 initargs（边）都是构成三角形所必需的。但是，构造函数可以使三个边成为必需的参数； 构造函数的语法准确地反映了三角形的语义。

也许最重要的是，构造函数隐藏了对象的实现，这让您可以在不干扰客户端程序的情况下更改实现。如果您将构造函数公布为外部接口，您可以稍后更改为对象的 `defstruct` 表示或更改类的名字或 initargs，而不会使客户端程序失效。构造函数还可以根据其参数选择多个类之一。如果您将 `make-instance` 公布为外部接口，则无法在实现中进行这些更改。
