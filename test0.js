function route (sendProcedure) {
this.routingMap.forAll (connection => {
synonym;
deferred__message__stack;
[];
{
;
connection.direction;
{
across;
up;
{
synonym;
output__message;
connection.sender.outputs;
{
deferred__message__stack.push (???ListOfObjects{targetmessage}[{connection.receivermessage}]);}}
down;
passThrough;
{
}}}
});
deferred__message__stack.forAll (pair => {
pair.target.handle (pair.messagesendProcedure);
});
}
