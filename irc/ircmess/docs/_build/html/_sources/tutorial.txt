Tutorial
========

Get the verb of a line
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: python

    ## getverb.py
    from ircmess import IRCLine

    LINE = ":Xe!yolo@swag.sage PRIVMSG #test :Hello"

    ircline = IRCLine(LINE)

    print ircline.verb

Get contents of a channel message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: python

    ## getchanmsg.py
    from ircmess import IRCLine

    LINE = ":Xe!yolo@swag.sage PRIVMSG #test :Hello"

    ircline = IRCLine(LINE)

    print ircline.args[0] + ":", ircline.args[-1]

Get source of a message
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: python

    ## get_source.py
    from ircmess import IRCLine

    LINE = ":Xe!yolo@swag.sage PRIVMSG #test :Hello"

    ircline = IRCLine(LINE)

    print ircline.source

