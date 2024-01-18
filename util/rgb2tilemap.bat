@rem FOR /F "tokens=*" %%g IN ('git rev-parse --show-toplevel') do (SET "gitdir=%%g")
@rem echo gitdir
@rem echo %%g
python C:\Users\comit\Dropbox\projects\AquariusPlus\util\rgb2tilemap.py %1 %2
