echo -e "Pushing build to github-pages"
# move the distributed source to home
cp dist/canvashs*.tar.gz $HOME/canvashs.tar.gz
cp -R dist/doc/html/canvashs/ $HOME/docs
cp -R jsdocs $HOME/jsdocs

# move to home
cd $HOME

# lets config git a little
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Travis"

# clone the pages repo to page
git clone --quiet --branch=master https://${GH_TOKEN}@github.com/CanvasHS/canvashs.github.io.git page > /dev/null 2>&1

# move to the distributions location on github pages
cd page/dist

# copy build to right direction
cp $HOME/canvashs.tar.gz canvashs-${TRAVIS_BUILD_NUMBER}.tar.gz

# also move the docs to here
cd docs
cp -R $HOME/docs $TRAVIS_BUILD_NUMBER

cd ../jsdocs
cp -R $HOME/jsdocs $TRAVIS_BUILD_NUMBER

# Go back to the root of the repository
cd $HOME/page

echo "
<tr>
    <td> #$TRAVIS_BUILD_NUMBER </td>
    <td> 0.1 </td> 
    <td> `date` </td>
    <td> <a href=\"dist/canvashs-$TRAVIS_BUILD_NUMBER.tar.gz\"> canvashs-$TRAVIS_BUILD_NUMBER.tar.gz </td> 
    <td> <a href=\"dist/docs/$TRAVIS_BUILD_NUMBER/index.html\"> hs docs </a> </td>
    <td> <a href=\"dist/jsdocs/$TRAVIS_BUILD_NUMBER/index.html\"> js docs </a> </td>
</tr>" | cat - _includes/list.html > /tmp/out && mv /tmp/out _includes/list.html

# push the changes back to github pages
git add .
git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to gh-pages"
git push -fq origin master > /dev/null 2>&1
if [ $? != 0 ]; then
    echo "Push to github failed"
fi

echo -e "pushToGithub: done"
