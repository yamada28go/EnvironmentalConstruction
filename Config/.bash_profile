# set PATH so it includes user's private bin if it exists                                                                                                      
if [ -d "$HOME/local/bin" ] ; then
    PATH="$HOME/local/bin:$PATH"
fi

if [ -d "$HOME/local/lib" ] ; then
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"$HOME/local/lib"
fi

