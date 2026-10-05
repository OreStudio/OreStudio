/* Harness: drives the three profile journeys over the BFF's HTTP surface
 * against the running stack -- present myself and keep my details current as a
 * member, change someone's details as a tenant administrator -- because the web
 * test harness renders without interaction and cannot exercise a save.
 *
 * It asserts what the journeys promise on the real server: the member's own
 * writes land through the self subjects, the read gap the journey documents
 * state refuses the member's account read while the contact read answers, the
 * picker's rule comes from the validator, the upload path answers a refused
 * image with its code, the reporting-line fields the administrator does not set
 * are echoed, and a contact write whose claim no longer holds is refused by the
 * store with its own code.
 *
 * Prerequisites:
 *   compass services start
 *   AUDIT_ACCOUNT=ores_brave_hopper_shell_user \
 *     projects/ores.web/scripts/seed-test-account.sh ores_web_probe 'Secure-Password-123'
 *   AUDIT_ACCOUNT=ores_brave_hopper_shell_user ROLE=Viewer \
 *     projects/ores.web/scripts/seed-test-account.sh ores_web_member 'Secure-Password-123'
 *
 * AUDIT_ACCOUNT names the account the audit columns record. The script's own
 * default, sysadmin, is not an account a recreated database holds, and the
 * seed refuses on the audit column before it writes anything.
 *
 * Run it from the repository root:
 *   node doc/agile/versions/v0/sprint_26/implement-profile-journeys/evidence/profile_journeys_check.js
 *
 * Exit code 0 means every assertion held. */

const zlib = require('zlib');

const BASE = process.env.ORES_WEB_URL ?? 'http://127.0.0.1:20402';
const MEMBER = { username: 'ores_web_member', password: 'Secure-Password-123' };
const ADMIN = { username: 'ores_web_probe', password: 'Secure-Password-123' };
const REASON = 'common.non_material_update';
const CONTACT_EMAIL = `${MEMBER.username}@contact.test`;

let failures = 0;
let checked = 0;

function check(label, condition, detail = '') {
    checked++;
    if (!condition) failures++;
    console.log(`  [${condition ? 'PASS' : 'FAIL'}] ${label}${detail.length > 0 ? ` (${detail})` : ''}`);
}

function group(title) {
    console.log(`\n${title}:`);
}

/** One HTTP client with a one-cookie jar, as a browser carries a session. */
function makeClient() {
    let cookie = '';

    async function request(method, path, body) {
        const headers = {};
        if (cookie.length > 0) headers.cookie = cookie;
        if (body !== undefined) headers['content-type'] = 'application/json';
        const response = await fetch(BASE + path, {
            method,
            headers,
            body: body === undefined ? undefined : JSON.stringify(body),
        });
        for (const entry of response.headers.getSetCookie()) {
            const pair = entry.split(';')[0];
            const [name, value] = pair.split('=');
            if (name === 'ores_web_session') cookie = value.length === 0 ? '' : pair;
        }
        const buffer = Buffer.from(await response.arrayBuffer());
        const contentType = response.headers.get('content-type') ?? '';
        const parsed =
            contentType.includes('json') && buffer.length > 0
                ? JSON.parse(buffer.toString('utf8'))
                : null;
        return { status: response.status, body: parsed, buffer, contentType };
    }

    return { request };
}

/** Signs in and picks a party when the server asks for one. */
async function signIn(client, who) {
    const login = await client.request('POST', '/api/session', {
        username: who.username,
        password: who.password,
    });
    check(`${who.username} signed in`, login.status === 200, `http ${login.status}`);
    if (login.status !== 200) return null;
    if (login.body.outcome === 'active') return login.body.session;

    const parties = login.body.availableParties ?? [];
    check(`${who.username} was offered a party`, parties.length > 0);
    const partyId = login.body.defaultPartyId ?? parties[0]?.id;
    const chosen = await client.request('POST', '/api/session/party', { partyId });
    check(`${who.username} chose a party`, chosen.status === 200, `http ${chosen.status}`);
    return chosen.status === 200 ? chosen.body : null;
}

const CRC_TABLE = (() => {
    const table = new Int32Array(256);
    for (let n = 0; n < 256; n++) {
        let c = n;
        for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
        table[n] = c;
    }
    return table;
})();

function crc32(bytes) {
    let c = 0xffffffff;
    for (const byte of bytes) c = CRC_TABLE[(c ^ byte) & 0xff] ^ (c >>> 8);
    return (c ^ 0xffffffff) >>> 0;
}

function chunk(type, data) {
    const head = Buffer.alloc(4);
    head.writeUInt32BE(data.length, 0);
    const body = Buffer.concat([Buffer.from(type, 'latin1'), data]);
    const crc = Buffer.alloc(4);
    crc.writeUInt32BE(crc32(body), 0);
    return Buffer.concat([head, body, crc]);
}

/**
 * A truecolour PNG of the given size. Level 0 stores the scanlines, so a
 * large image stays large and exercises the validator's size rule.
 */
function png(width, height, level) {
    const stride = 1 + width * 3;
    const raw = Buffer.alloc(height * stride);
    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            const at = y * stride + 1 + x * 3;
            raw[at] = (x * 7 + y * 3) % 256;
            raw[at + 1] = (x * 3 + y * 11) % 256;
            raw[at + 2] = (x * 13 + y * 5) % 256;
        }
    }
    const ihdr = Buffer.alloc(13);
    ihdr.writeUInt32BE(width, 0);
    ihdr.writeUInt32BE(height, 4);
    ihdr[8] = 8;
    ihdr[9] = 2;
    return Buffer.concat([
        Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]),
        chunk('IHDR', ihdr),
        chunk('IDAT', zlib.deflateSync(raw, { level })),
        chunk('IEND', Buffer.alloc(0)),
    ]);
}

async function memberJourney() {
    group("Present myself and keep my details current, as the member");
    const client = makeClient();
    const session = await signIn(client, MEMBER);
    if (session === null) return null;
    const accountId = session.accountId;

    group('the grants the session carries');
    const access = await client.request('GET', '/api/me/access');
    check('the grants read answers', access.status === 200, `http ${access.status}`);
    const roles = access.body?.roles ?? [];
    const names = roles.map((role) => role.name);
    check('the session carries the Viewer role', names.includes('Viewer'), names.join(', '));
    const codes = roles.flatMap((role) => role.permissionCodes ?? []);
    check(
        'no held grant carries iam::accounts:read',
        !codes.includes('iam::accounts:read'),
        codes.join(', '),
    );

    group('the read gap the journey documents state');
    const refusedRead = await client.request('GET', `/api/accounts/${MEMBER.username}`);
    check(
        "the member's read of their own account is refused",
        refusedRead.status === 403,
        `http ${refusedRead.status} ${refusedRead.body?.code ?? ''}`,
    );
    check('the refusal carries the server code', refusedRead.body?.code === 'forbidden');

    group("the member's own contact record, which carries no guard");
    const before = await client.request('GET', '/api/me/contact-information');
    check('the own contact read answers', before.status === 200, `http ${before.status}`);
    const beforeContact = before.body?.contact ?? null;
    check(
        "a record, when present, belongs to the session's account",
        beforeContact === null || beforeContact.accountId === accountId,
    );

    group('the self profile write');
    const profile = await client.request('PUT', '/api/me/profile', {
        fullName: 'Ada Lovelace Member',
        jobTitle: 'Profile verifier',
        imageId: '',
        reasonCode: REASON,
        commentary: '',
    });
    check('the self write answers', profile.status === 200, `http ${profile.status}`);
    check('the write was accepted', profile.body?.result?.outcome === 'ok', profile.body?.result?.message ?? '');
    check('the account carries the written name', profile.body?.account?.fullName === 'Ada Lovelace Member');
    check('the account carries the written job title', profile.body?.account?.jobTitle === 'Profile verifier');
    check("the account names the session's account", profile.body?.account?.id === accountId);

    group('the self contact write');
    const contact = await client.request('PUT', '/api/me/contact-information', {
        streetLine1: '1 Analytical Engine Way',
        streetLine2: 'Suite 42',
        city: 'London',
        state: 'Greater London',
        countryCode: 'GB',
        postalCode: 'EC1A 1BB',
        phone: '+44 20 7946 0958',
        email: CONTACT_EMAIL,
        webPage: 'https://example.test/ada',
        reasonCode: REASON,
        commentary: '',
    });
    check('the contact write answers', contact.status === 200, `http ${contact.status}`);
    check('the write was accepted', contact.body?.result?.outcome === 'ok', contact.body?.result?.message ?? '');
    const record = contact.body?.contact ?? null;
    check("the record names the session's account", record?.accountId === accountId);
    check('the record carries the written street', record?.streetLine1 === '1 Analytical Engine Way');
    check('the record carries the written contact address', record?.email === CONTACT_EMAIL);
    check('the write bumped the record the read showed', (record?.version ?? -1) > (beforeContact?.version ?? -1));

    group('the round trip');
    const readBack = await client.request('GET', '/api/me/contact-information');
    check('the record reads back with the city', readBack.body?.contact?.city === 'London');
    check('the record reads back unchanged', readBack.body?.contact?.version === record?.version);

    group("the picker's rule, read from the validator");
    const policy = await client.request('GET', '/api/image-upload-policy');
    check('the policy read answers', policy.status === 200, `http ${policy.status}`);
    check(
        'the largest size is two megabytes',
        policy.body?.maxSizeBytes === 2 * 1024 * 1024,
        String(policy.body?.maxSizeBytes),
    );
    check('the smallest side is 128', policy.body?.minWidth === 128 && policy.body?.minHeight === 128);
    check('the formats are stated', (policy.body?.formats ?? []).length > 0, (policy.body?.formats ?? []).join(', '));

    group('the photo upload and a refusal');
    const image = png(128, 128, 6);
    const uploaded = await client.request('POST', '/api/images', {
        mimeType: 'image/png',
        data: image.toString('base64'),
    });
    check('the upload answers', uploaded.status === 200, `http ${uploaded.status}`);
    check('the upload was accepted', uploaded.body?.result?.outcome === 'ok', uploaded.body?.result?.message ?? '');
    const imageId = uploaded.body?.imageId ?? '';
    check('the upload answers an identifier', /^[0-9a-f-]{36}$/.test(imageId), imageId);

    const oversized = png(900, 900, 0);
    check('the oversized image is over the limit', oversized.length > 2 * 1024 * 1024, `${oversized.length} bytes`);
    const refusedImage = await client.request('POST', '/api/images', {
        mimeType: 'image/png',
        data: oversized.toString('base64'),
    });
    check('the oversized upload answers rather than fails', refusedImage.status === 200, `http ${refusedImage.status}`);
    check(
        'the refusal names the size rule',
        refusedImage.body?.result?.code === 'image_too_large',
        refusedImage.body?.result?.code ?? '',
    );

    const withPhoto = await client.request('PUT', '/api/me/profile', {
        fullName: 'Ada Lovelace Member',
        jobTitle: 'Profile verifier',
        imageId,
        reasonCode: REASON,
        commentary: '',
    });
    check('the photo write was accepted', withPhoto.body?.result?.outcome === 'ok', withPhoto.body?.result?.message ?? '');
    check('the account carries the uploaded picture', withPhoto.body?.account?.imageId === imageId);

    group("the administrator's route refuses the member");
    const trespass = await client.request('PUT', `/api/accounts/${ADMIN.username}/profile`, {
        fullName: 'Not Allowed',
        jobTitle: '',
        imageId: '',
        reasonCode: REASON,
        commentary: '',
    });
    check('the write is refused', trespass.status === 403, `http ${trespass.status} ${trespass.body?.code ?? ''}`);

    return { accountId, imageId, image };
}

async function adminJourney(member) {
    group("Change someone's details, as the tenant administrator");
    const client = makeClient();
    const session = await signIn(client, ADMIN);
    if (session === null) return;

    group("the picker's read");
    const page = await client.request('GET', '/api/accounts?limit=100');
    check('the tenant list answers', page.status === 200, `http ${page.status}`);
    const listed = (page.body?.accounts ?? []).find((account) => account.username === MEMBER.username);
    check('the list holds the member', listed !== undefined);

    group('the account read the member was refused');
    const read = await client.request('GET', `/api/accounts/${MEMBER.username}`);
    check("the administrator's account read answers", read.status === 200, `http ${read.status}`);
    const before = read.body?.account ?? null;
    check("the read names the member's account", before?.id === member.accountId);
    check("the read shows the member's picture", before?.imageId === member.imageId);

    group("the administrator's profile write, the unset fields echoed");
    const write = await client.request('PUT', `/api/accounts/${MEMBER.username}/profile`, {
        fullName: 'Ada Lovelace',
        jobTitle: 'Mathematician',
        imageId: member.imageId,
        reasonCode: REASON,
        commentary: '',
    });
    check('the administrator write answers', write.status === 200, `http ${write.status}`);
    check('the write succeeded', write.body?.success === true);
    const after = await client.request('GET', `/api/accounts/${MEMBER.username}`);
    check('the name moved', after.body?.account?.fullName === 'Ada Lovelace');
    check('the job title moved', after.body?.account?.jobTitle === 'Mathematician');
    check('the sign-in address stayed', after.body?.account?.email === before?.email, after.body?.account?.email ?? '');
    check('the default party stayed', after.body?.account?.defaultPartyId === before?.defaultPartyId);
    check('the reporting line stayed', after.body?.account?.reportsToAccountId === before?.reportsToAccountId);
    check('the version moved forward', (after.body?.account?.version ?? -1) > (before?.version ?? -1));

    group("the contact write with the claim the panel read");
    const readContact = await client.request('GET', `/api/accounts/${member.accountId}/contact-information`);
    check("the administrator's contact read answers", readContact.status === 200, `http ${readContact.status}`);
    const contact = readContact.body?.contact ?? null;
    check('the record the member wrote is the one read', contact?.email === CONTACT_EMAIL);
    const version = contact?.version ?? -1;

    const writeContact = (fields, claim) =>
        client.request('PUT', `/api/accounts/${member.accountId}/contact-information`, {
            streetLine1: '2 Analytical Engine Way',
            streetLine2: '',
            city: 'London',
            state: '',
            countryCode: 'GB',
            postalCode: 'EC1A 2CC',
            phone: '+44 20 7946 0959',
            email: CONTACT_EMAIL,
            webPage: 'https://example.test/ada',
            reasonCode: REASON,
            commentary: '',
            ...fields,
            version: claim,
        });

    const claimed = await writeContact({ streetLine2: 'Floor 3' }, version);
    check('the claimed write answers', claimed.status === 200, `http ${claimed.status}`);
    check('the claim held', claimed.body?.result?.outcome === 'ok', claimed.body?.result?.message ?? '');
    check('the street moved', claimed.body?.contact?.streetLine2 === 'Floor 3');
    check('the version moved forward', (claimed.body?.contact?.version ?? -1) > version);

    group('a claim that no longer holds');
    const stale = await writeContact({ streetLine1: '3 Analytical Engine Way' }, version);
    check('the stale claim is answered, not failed', stale.status === 200, `http ${stale.status}`);
    check('the stale claim is a conflict', stale.body?.result?.outcome === 'conflict', stale.body?.result?.outcome ?? '');
    check('the conflict names the version', stale.body?.result?.code === 'version_conflict', stale.body?.result?.code ?? '');
    check('the conflict carries a message', (stale.body?.result?.message ?? '').length > 0, stale.body?.result?.message ?? '');
    check('no record came back with the refusal', (stale.body?.contact ?? null) === null);
    const untouched = await client.request('GET', `/api/accounts/${member.accountId}/contact-information`);
    check('the refused write changed nothing', untouched.body?.contact?.streetLine1 === '2 Analytical Engine Way');

    group('a claim of no record where one exists');
    const noRecord = await writeContact({}, null);
    check('the no-record claim is a conflict', noRecord.body?.result?.outcome === 'conflict', noRecord.body?.result?.outcome ?? '');
    check('the conflict names the existing row', noRecord.body?.result?.code === 'already_exists', noRecord.body?.result?.code ?? '');

    group('a claim of no record where none exists');
    const adminContact = await client.request('GET', `/api/accounts/${session.accountId}/contact-information`);
    if ((adminContact.body?.contact ?? null) === null) {
        const inserted = await client.request('PUT', `/api/accounts/${session.accountId}/contact-information`, {
            streetLine1: '4 Analytical Engine Way',
            streetLine2: '',
            city: 'London',
            state: '',
            countryCode: 'GB',
            postalCode: 'EC1A 4DD',
            phone: '+44 20 7946 0960',
            email: `${ADMIN.username}@contact.test`,
            webPage: '',
            reasonCode: REASON,
            commentary: '',
            version: null,
        });
        check('the no-record claim inserts one', inserted.body?.result?.outcome === 'ok', inserted.body?.result?.message ?? '');
        check("the inserted record names the administrator's account", inserted.body?.contact?.accountId === session.accountId);
    } else {
        console.log('  [note] the administrator has a record from an earlier run; the insert path was checked then');
    }

    group('the picture the account points at');
    const picture = await client.request('GET', `/api/accounts/${MEMBER.username}/picture`);
    check('the picture answers', picture.status === 200, `http ${picture.status}`);
    check('the picture is a png', picture.contentType.includes('image/png') || picture.contentType.includes('image/PNG'), picture.contentType);
    check('the picture is the uploaded bytes', Buffer.compare(picture.buffer, member.image) === 0, `${picture.buffer.length} bytes`);
}

async function main() {
    console.log(`profile journeys over ${BASE}`);
    const member = await memberJourney();
    if (member !== null) await adminJourney(member);
    console.log(`\n${checked} checks ran`);
    console.log(failures === 0 ? 'ALL CHECKS PASSED' : `${failures} CHECK(S) FAILED`);
    return failures === 0 ? 0 : 1;
}

main().then(
    (code) => process.exit(code),
    (error) => {
        console.error('\nverification aborted:', error);
        process.exit(1);
    },
);
