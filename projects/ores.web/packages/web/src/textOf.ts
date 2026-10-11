/** The words of a rendered page: its markup removed, so a test reads what a person reads. */
export function textOf(html: string): string {
    return html
        .replace(/<[^>]*>/g, '')
        .replace(/&#x27;/g, "'")
        .replace(/&quot;/g, '"')
        .replace(/&lt;/g, '<')
        .replace(/&gt;/g, '>')
        .replace(/&amp;/g, '&');
}
