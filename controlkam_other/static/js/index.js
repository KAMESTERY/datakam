"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ctrlkm_grpc_web_pb_1 = require("./grpc/ctrlkm_grpc_web_pb");
var ctrlkm_pb_1 = require("./grpc/ctrlkm_pb");
var client = new ctrlkm_grpc_web_pb_1.ControlKamClient('localhost:1991', {}, {});
var uc = new ctrlkm_pb_1.UserCredentials();
var ac = new ctrlkm_pb_1.AuthClaims();
client.login(uc, {}, function (err, resp) {
    ac = resp;
});
//# sourceMappingURL=index.js.map