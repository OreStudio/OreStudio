/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { useState, type ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { z } from 'zod';
import { request } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, cx } from '../ui/Primitives.js';

/**
 * The image an entity carries, and how to change it.
 *
 * The Qt dialog had this and the interface did not: a record with a picture
 * showed nothing, and there was no way to give it one. Some entities carry an
 * image rather than a field — a country's flag, a party's logo — so this is a
 * shared concern rather than something each entity invents.
 *
 * The picker lists metadata and fetches only the chosen image's bytes. A grid of
 * six hundred flags that downloaded six hundred flags would be a picker nobody
 * opens twice.
 */

const imageInfoSchema = z.object({
  imageId: z.string(),
  key: z.string(),
  description: z.string(),
  sizeBytes: z.int(),
});
const imageListSchema = z.object({ images: z.array(imageInfoSchema) });

type ImageInfo = z.infer<typeof imageInfoSchema>;

function useImages(enabled: boolean): ReturnType<typeof useQuery<readonly ImageInfo[]>> {
  return useQuery({
    queryKey: ['images'],
    enabled,
    queryFn: async () => {
      const body = await request('/api/images', { method: 'GET' });
      return imageListSchema.parse(body).images;
    },
    // The set changes rarely and the picker is opened occasionally.
    staleTime: 5 * 60 * 1000,
  });
}

export function FlagEditor({
  imageId,
  editable,
  filter,
  onPick,
}: {
  readonly imageId: string | undefined;
  readonly editable: boolean;
  /**
   * What to narrow the choices to.
   *
   * The images are not tagged by kind, but the flags follow a convention: the key
   * is the country code and the description reads "Flag of xx". Matching on that
   * is what keeps a flag picker from offering a staff photograph.
   */
  readonly filter?: string;
  readonly onPick: (imageId: string | null) => void;
}): ReactNode {
  const { t } = useTranslation();
  const [picking, setPicking] = useState(false);
  const hasImage = imageId !== undefined && imageId.length > 0;

  return (
    <div className="mb-5 flex items-center gap-3">
      {/*
        The image is the button.
        
        A separate "change image" control is a second thing to find for an act
        that only applies to the image, and it leaves the image itself looking
        like decoration. Clicking the picture to change the picture is what
        anybody tries first.
      */}
      <button
        type="button"
        disabled={!editable}
        onClick={() => setPicking(true)}
        title={editable ? (hasImage ? t('image.change') : t('image.choose')) : undefined}
        className={cx(
          'grid size-12 shrink-0 place-items-center overflow-hidden rounded-md border border-line bg-bg-secondary',
          editable && 'cursor-pointer hover:border-line-strong hover:bg-surface-overlay',
        )}
      >
        {hasImage ? (
          <img
            src={`/api/images/${encodeURIComponent(imageId)}`}
            alt=""
            className="max-h-full max-w-full object-contain"
          />
        ) : (
          // Not an empty box with no explanation: a record without an image says
          // that it has none, and that it can be given one.
          <span className="px-1 text-center text-[10px] leading-tight text-ink-faint">
            {editable ? t('image.choose') : t('image.none')}
          </span>
        )}
      </button>

      {editable && hasImage && (
        <Button variant="ghost" size="sm" onClick={() => onPick(null)}>
          {t('image.remove')}
        </Button>
      )}

      {picking && (
        <FlagPicker
          current={imageId}
          {...(filter === undefined ? {} : { filter })}
          onPick={(chosen) => {
            onPick(chosen);
            setPicking(false);
          }}
          onClose={() => setPicking(false)}
        />
      )}
    </div>
  );
}

/** The images to choose from, as a grid. */
function FlagPicker({
  current,
  filter,
  onPick,
  onClose,
}: {
  readonly current: string | undefined;
  readonly filter?: string;
  readonly onPick: (imageId: string | null) => void;
  readonly onClose: () => void;
}): ReactNode {
  const { t } = useTranslation();
  const [search, setSearch] = useState('');
  const query = useImages(true);

  const images = (query.data ?? []).filter((image) => {
    const haystack = `${image.key} ${image.description}`.toLowerCase();
    if (filter !== undefined && !haystack.includes(filter.toLowerCase())) return false;
    const needle = search.trim().toLowerCase();
    return needle.length === 0 || haystack.includes(needle);
  });

  return (
    <Dialog title={t('image.pick')} onClose={onClose} wide>
      {query.isPending ? (
        <p className="py-6 text-center text-sm text-ink-faint">{t('entity.loading')}</p>
      ) : query.isError ? (
        <p className="py-6 text-center text-sm text-ink-muted">{t('feedback.unreachable')}</p>
      ) : images.length === 0 ? (
        <p className="py-6 text-center text-sm text-ink-muted">{t('image.noneAvailable')}</p>
      ) : (
        <>
          <input
            type="search"
            value={search}
            autoFocus
            onChange={(event) => setSearch(event.target.value)}
            placeholder={t('image.search')}
            aria-label={t('image.search')}
            className="mb-3 h-9 w-full rounded-md border border-line bg-bg-secondary px-2.5 text-sm text-ink placeholder:text-ink-faint focus:border-line-strong focus:outline-none"
          />
          <div className="grid max-h-96 grid-cols-[repeat(auto-fill,minmax(84px,1fr))] gap-2 overflow-y-auto">
            {images.map((image) => (
              <button
                key={image.imageId}
                type="button"
                onClick={() => onPick(image.imageId)}
                aria-current={image.imageId === current}
                title={image.description.length > 0 ? image.description : image.key}
                className={cx(
                  'flex h-16 items-center justify-center rounded-md border p-1.5 transition-colors',
                  image.imageId === current
                    ? 'border-accent bg-surface-overlay'
                    : 'border-line bg-bg-secondary hover:border-line-strong',
                )}
              >
                <img
                  src={`/api/images/${encodeURIComponent(image.imageId)}`}
                  alt={image.key}
                  loading="lazy"
                  className="max-h-full max-w-full object-contain"
                />
              </button>
            ))}
          </div>
        </>
      )}

      <div className="mt-4 flex justify-end gap-2">
        <Button variant="ghost" size="md" onClick={onClose}>
          {t('entity.close')}
        </Button>
      </div>
    </Dialog>
  );
}
