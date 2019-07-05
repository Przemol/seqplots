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

##### Sync branch with GitHub - new interface

```

git pull
git fetch --all
git merge upstream/master
#git merge --allow-unrelated-histories upstream/master

git push origin master
git merge upstream/master

git merge origin/RELEASE_3_9
git merge upstream/RELEASE_3_9

git push origin RELEASE_3_9
git push upstream RELEASE_3_9

```





