# Devel branch

##### Stop any previous rebase 
```
git rebase --abort
```

##### Sync with Bioc
```
git branch devel
git reset --hard  bioc/master
git svn rebase
git svn dcommit --add-author-from
```

##### Add local changes from master 
```
git merge master --log
git svn dcommit --add-author-from
```

##### Sync with GitHub
```
git push --force
```




