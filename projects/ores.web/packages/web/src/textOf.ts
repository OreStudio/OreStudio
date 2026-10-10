/** The words of a rendered page: its markup removed, so a test reads what a person reads. */
export function textOf(html: string): string {
    return html.replace(/<[^>]*>/g, '');
}
