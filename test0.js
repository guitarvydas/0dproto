function route (sendProcedure) {
{
var deferred__message__stack = [];
this.routingMap.forAll (connection => {
;
connection.direction;
{
across;
up;
{
{
var output__message = connection.sender.outputs;
deferred__message__stack.push (???ListOfObjects{targetmessage}[{connection.receivermessage}]);
}}
down;
passThrough;
{
}}
});
deferred__message__stack.forAll (pair => {
pair.target.handle (pair.messagesendProcedure);
});
}
}
