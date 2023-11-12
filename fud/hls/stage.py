from fud.stages import SourceType, Stage
from fud.utils import shell

class FPBenchStage(Stage):
    name = 'fpbench-tools'

    def __init__(self):
        super().__init__(
            src_state='fpcore',
            target_state='vivado-hls',
            input_type=SourceType.Stream,
            output_type=SourceType.Stream,
            description='Compile FPCore to Vivado C++',
        )

    @staticmethod
    def pre_install():
        pass

    @staticmethod
    def defaults():
        return {'exec': 'racket'}

    def known_opts(self):
        return ['exec', 'exporter', 'lang']

    def _define_steps(self, input, builder, config):
        racket = config['stages', self.name, 'exec']
        exporter = config['stages', self.name, 'exporter']
        lang = config.get(['stages', self.name, 'lang']) or 'vivado'

        cmd = ' '.join([racket, exporter, '--lang', lang, '- -'])

        @builder.step(description=cmd)
        def run_exporter(stream: SourceType.Stream) -> SourceType.Stream:
            return shell(cmd, stdin=stream)

        return run_exporter(input)

__STAGES__ = [FPBenchStage]
