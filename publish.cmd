npm run build && git push . head:publish -f && git checkout publish && move /Y dist\* . && mkdir img && move /Y dist\img\* .\img && git add . && git commit -m publish && git push origin publish -f && git checkout main