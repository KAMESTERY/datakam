using Amazon.CDK;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Devops
{
    sealed class Program
    {
        public static void Main(string[] args)
        {
            var app = new App();
            new DevopsStack(app, "KamesteryStack");
            app.Synth();
        }
    }
}
