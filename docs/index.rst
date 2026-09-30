.. meta::
   :description:
      Threedx generates interpretable, probabilistic forecasts.
   :keywords: forecasts, interpretable, probabilistic, sample paths

threedx
========================================================

.. toctree::
   :maxdepth: 1
   :hidden:
   :titlesonly:

   Home <self>

..  rubric:: Robust and interpretable probabilistic forecasts.

----------

Threedx provides an interface for interpretable probabilistic forecasts that
puts you in control.

..  sidebar::

    :doc:`Nothing but numpy <howto/install>` as dependencies.

If you believe in choosing the right model for the job, you'll love that Threedx
let's you pick the loss function at training time. And at prediction time,
Threedx either forecasts by sampling from past observations, or via an
innovations generating process you define.

Threedx could be the right fit for you, if...

- your users require interpretability,
- your use case benefits from a custom loss function,
- your data is dominated by seasonality while trends are weak,
- your data is intermittent, non-negative, or otherwise non-normal.

Contents
--------

The best way to learn about Threedx is by diving in:

..  toctree::
   :maxdepth: 1
   :titlesonly:

   howto/install
   howto/getstarted

Afterwards, check these tutorials to see different ways in which Threedx can be
applied.

..  rst-class:: toc-with-header

..  toctree::
   :maxdepth: 1
   :titlesonly:

   tutorials/README
   Forecast fev-bench Tasks Using Threedx <tutorials/forecast_fev_bench_tasks_using_threedx>

For more in-depth background on Threedx and the concepts behind Threedx, refer
to the following sections.

..  rst-class:: toc-with-header

..  toctree::
   :maxdepth: 1
   :titlesonly:

   concepts/README

Finally, the API reference.

..  toctree::
   :maxdepth: 1
   :titlesonly:

   Reference <api/threedx/threedx>

