echo -e "Pushing build to github-pages"
# move the distributed source to home
cp dist/canvashs*.tar.gz $HOME/canvashs.tar.gz
# move to home
cd $HOME
# lets config git a little
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Travis"
# clone the pages repo to gh-pages
git clone --quiet --branch=master https://${GH_TOKEN}@github.com/CanvasHS/canvashs.github.io.git page > /dev/null
#change dir to dist
cd page/dist

#copy build to right direction
cp $HOME/canvashs.tar.gz canvashs-${TRAVIS_BUILD_NUMBER}.tar.gz
echo "<tr><td> #$TRAVIS_BUILD_NUMBER </td><td> 0.1 </td> <td> `date` </td><td> <a href=\"dist/canvashs-$TRAVIS_BUILD_NUMBER.tar.gz\"> canvashs-$TRAVIS_BUILD_NUMBER.tar.gz </td></tr>" >> ../_includes/list.html
cd ..
git add .
git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to gh-pages"
git push -fq origin master > /dev/null

echo -e "build pushed to gh-pages"
