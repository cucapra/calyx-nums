from pathlib import Path

import fud.config
from fud.stages import SourceType, Stage
from fud.utils import shell

class CalyxNumbersStage(Stage):
    name = 'calyx-nums'

    def __init__(self):
        super().__init__(
            src_state='fpcore',
            target_state='calyx',
            input_type=SourceType.Stream,
            output_type=SourceType.Stream,
            description='Compile FPCore to Calyx',
        )

    @staticmethod
    def pre_install():
        pass

    @staticmethod
    def defaults():
        root = Path(__file__).parent.parent
        exe = root / 'target' / 'debug' / 'calyx-nums'

        return {'exec': str(exe.resolve()), 'file_extensions': ['.fpcore']}

    def known_opts(self):
        return ['exec', 'file_extensions', 'flags']

    def _define_steps(self, input, builder, config):
        exe = config['stages', self.name, 'exec']
        flags = config.get(['stages', self.name, 'flags']) or ''

        cmd = ' '.join([exe, '-l', config['global', fud.config.ROOT], flags])

        @builder.step(description=cmd)
        def run_calyx_nums(stream: SourceType.Stream) -> SourceType.Stream:
            return shell(cmd, stdin=stream)

        return run_calyx_nums(input)

__STAGES__ = [CalyxNumbersStage]
