import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { RefreshButton } from './RefreshButton.js';

function draw(props: {
    stale?: boolean;
    changedAt?: string;
    dataUpdatedAt?: number;
    oldestUpdatedAt?: number;
}): string {
    const { stale = false, changedAt, dataUpdatedAt = 0, oldestUpdatedAt } = props;
    return renderToStaticMarkup(
        <TranslationProvider>
            <RefreshButton
                onClick={() => undefined}
                load={{
                    dataUpdatedAt,
                    isFetching: false,
                    ...(oldestUpdatedAt === undefined ? {} : { oldestUpdatedAt }),
                }}
                news={{ stale, changedAt }}
            />
        </TranslationProvider>,
    );
}

describe('the Refresh button', () => {
    it('is plain when nothing changed', () => {
        const html = draw({});
        expect(html).toContain('Refresh');
        expect(html).not.toContain('stale-mark');
        expect(html).not.toContain('stale-pulse');
    });

    it('turns gold, pulses and says why when the data is stale', () => {
        const html = draw({ stale: true });
        expect(html).toContain('stale-mark');
        expect(html).toContain('stale-pulse');
        expect(html).toContain('The data changed on the server. Refresh to see it.');
    });

    it('states the time of the change in the tooltip when it is known', () => {
        const html = draw({ stale: true, changedAt: '2026-10-10 12:00:03Z' });
        expect(html).toContain('The data changed on the server at ');
    });

    it('says nothing about a load before the data has arrived', () => {
        expect(draw({})).not.toContain('<time');
    });

    it('says when the data was loaded as an ISO timestamp, faintly, under the button', () => {
        const html = draw({ dataUpdatedAt: new Date(2026, 9, 11, 9, 3, 22).getTime() });
        expect(html).toMatch(/>2026-10-11T09:03:22[+-]\d{2}:\d{2}</);
        expect(html).toContain('opacity-50');
        expect(html).not.toContain('>Loaded');
    });

    it('names what the timestamp is in the tooltip', () => {
        const html = draw({ dataUpdatedAt: new Date(2026, 9, 11, 9, 3, 22).getTime() });
        expect(html).toMatch(/title="Loaded 2026-10-11T09:03:22/);
    });

    it('states the oldest of several reads, not the newest', () => {
        const html = draw({
            dataUpdatedAt: new Date(2026, 9, 11, 9, 9, 0).getTime(),
            oldestUpdatedAt: new Date(2026, 9, 11, 9, 3, 22).getTime(),
        });
        expect(html).toContain('T09:03:22');
        expect(html).not.toContain('T09:09:00');
    });
});
