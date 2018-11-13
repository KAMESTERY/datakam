/**
 * @fileoverview gRPC-Web generated client stub for ctrlkm
 * @enhanceable
 * @public
 */

// GENERATED CODE -- DO NOT EDIT!



const grpc = {};
grpc.web = require('grpc-web');

const proto = {};
proto.ctrlkm = require('./ctrlkm_pb.js');

/**
 * @param {string} hostname
 * @param {?Object} credentials
 * @param {?Object} options
 * @constructor
 * @struct
 * @final
 */
proto.ctrlkm.ControlKamClient =
    function(hostname, credentials, options) {
  if (!options) options = {};
  options['format'] = 'text';

  /**
   * @private @const {!grpc.web.GrpcWebClientBase} The client
   */
  this.client_ = new grpc.web.GrpcWebClientBase(options);

  /**
   * @private @const {string} The hostname
   */
  this.hostname_ = hostname;

  /**
   * @private @const {?Object} The credentials to be used to connect
   *    to the server
   */
  this.credentials_ = credentials;

  /**
   * @private @const {?Object} Options for the client
   */
  this.options_ = options;
};


/**
 * @param {string} hostname
 * @param {?Object} credentials
 * @param {?Object} options
 * @constructor
 * @struct
 * @final
 */
proto.ctrlkm.ControlKamPromiseClient =
    function(hostname, credentials, options) {
  if (!options) options = {};
  options['format'] = 'text';

  /**
   * @private @const {!proto.ctrlkm.ControlKamClient} The delegate callback based client
   */
  this.delegateClient_ = new proto.ctrlkm.ControlKamClient(
      hostname, credentials, options);

};


/**
 * @const
 * @type {!grpc.web.AbstractClientBase.MethodInfo<
 *   !proto.ctrlkm.UserCredentials,
 *   !proto.ctrlkm.AuthClaims>}
 */
const methodInfo_Login = new grpc.web.AbstractClientBase.MethodInfo(
  proto.ctrlkm.AuthClaims,
  /** @param {!proto.ctrlkm.UserCredentials} request */
  function(request) {
    return request.serializeBinary();
  },
  proto.ctrlkm.AuthClaims.deserializeBinary
);


/**
 * @param {!proto.ctrlkm.UserCredentials} request The
 *     request proto
 * @param {!Object<string, string>} metadata User defined
 *     call metadata
 * @param {function(?grpc.web.Error, ?proto.ctrlkm.AuthClaims)}
 *     callback The callback function(error, response)
 * @return {!grpc.web.ClientReadableStream<!proto.ctrlkm.AuthClaims>|undefined}
 *     The XHR Node Readable Stream
 */
proto.ctrlkm.ControlKamClient.prototype.login =
    function(request, metadata, callback) {
  return this.client_.rpcCall(this.hostname_ +
      '/ctrlkm.ControlKam/Login',
      request,
      metadata,
      methodInfo_Login,
      callback);
};


/**
 * @param {!proto.ctrlkm.UserCredentials} request The
 *     request proto
 * @param {!Object<string, string>} metadata User defined
 *     call metadata
 * @return {!Promise<!proto.ctrlkm.AuthClaims>}
 *     The XHR Node Readable Stream
 */
proto.ctrlkm.ControlKamPromiseClient.prototype.login =
    function(request, metadata) {
  return new Promise((resolve, reject) => {
    this.delegateClient_.login(
      request, metadata, (error, response) => {
        error ? reject(error) : resolve(response);
      });
  });
};


module.exports = proto.ctrlkm;

