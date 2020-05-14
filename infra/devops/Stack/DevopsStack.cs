using Amazon.CDK;
using Amazon.CDK.AWS.S3;

namespace Devops
{
    public class DevopsStack : Stack
    {
        internal DevopsStack(Construct scope, string id, IStackProps props = null) : base(scope, id, props)
        {
            // The code that defines your stack goes here
            var datakamBucket = new Bucket(this, "DataKam");
        }
    }
}
