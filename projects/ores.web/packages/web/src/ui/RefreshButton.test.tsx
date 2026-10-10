import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { RefreshButton } from './RefreshButton.js';

function draw(props: { stale?: boolean; changedAt?: string }): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <RefreshButton onClick={() => undefined} {...props} />
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
        const html = draw({ stale: true, changedAt: '2026-10-10T12:00:03Z' });
        expect(html).toContain('The data changed on the server at ');
    });
});
