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

import { z } from 'zod';

/**
 * An image, and the flags that are images.
 *
 * Flags are not a separate concept in ORE Studio: a country carries an
 * `image_id`, and the image is an SVG in the assets service. That is why there
 * is no "flag" field anywhere — there is a reference to an image, and the image
 * happens to be a flag.
 *
 * The bytes arrive in whatever form the codec chose for a byte vector, so all
 * three plausible shapes are accepted and normalised at the boundary. Guessing
 * one would produce a route that works against one codec and silently returns
 * nothing against another.
 */
const imageBytes = z.union([
  z.string(),
  z.array(z.number().int().min(0).max(255)),
  z.instanceof(Uint8Array),
]);

export const imageSchema = z.object({
  version: z.int().nonnegative().default(0),
  image_id: z.string().default(''),
  tenant_id: z.string().default(''),
  key: z.string().default(''),
  description: z.string().default(''),
  mime_type: z.string().default('image/svg+xml'),
  data: imageBytes,
  modified_by: z.string().default(''),
  change_reason_code: z.string().default(''),
  change_commentary: z.string().default(''),
  performed_by: z.string().default(''),
  recorded_at: z.string().default(''),
});

export type WireImage = z.infer<typeof imageSchema>;

export const getImagesRequestSchema = z.object({
  image_ids: z.array(z.string()),
});

export const getImagesResponseSchema = z.object({
  success: z.boolean().default(true),
  message: z.string().default(''),
  images: z.array(imageSchema).default([]),
});

/**
 * The bytes, as a buffer.
 *
 * A string is already the image text, an array of numbers is the bytes one by
 * one, and a `Uint8Array` is the codec's own binary type. All three end up the
 * same way, so nothing downstream has to care which arrived.
 */
export function imageBytesToBuffer(data: WireImage['data']): Buffer {
  if (typeof data === 'string') return Buffer.from(data, 'binary');
  if (data instanceof Uint8Array) return Buffer.from(data);
  return Buffer.from(data);
}

/**
 * The bytes as text.
 *
 * Most of these images are SVG, which is text, so the useful form for an
 * inspection or a test is the markup rather than a byte count.
 */
export function imageBytesToText(data: WireImage['data']): string {
  if (typeof data === 'string') return data;
  return Buffer.from(data instanceof Uint8Array ? data : Uint8Array.from(data)).toString('utf8');
}

/**
 * An image's metadata, without its bytes.
 *
 * What a picker needs: enough to show a grid of things to choose from, and not
 * the contents of every one of them. The bytes are fetched only for the chosen
 * image, which is what keeps a picker over six hundred flags from being six
 * hundred downloads.
 */
export const imageInfoSchema = z.object({
  image_id: z.string(),
  key: z.string().default(''),
  description: z.string().default(''),
  size_bytes: z.int().nonnegative().default(0),
});

export type WireImageInfo = z.infer<typeof imageInfoSchema>;

/**
 * The list call.
 *
 * `modified_since` has no default because the C++ struct has none and the
 * decoder requires every member; null means everything.
 */
export const listImagesRequestSchema = z.object({
  modified_since: z.string().nullable(),
});

export const listImagesResponseSchema = z.object({
  success: z.boolean().default(true),
  message: z.string().default(''),
  images: z.array(imageInfoSchema).default([]),
});
