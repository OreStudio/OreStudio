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
 * The country wire shape.
 *
 * Field names are the C++ member names, because they are the keys `rfl::msgpack`
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * This file is what the code generator will emit for every entity. It is written
 * by hand once, for one entity, to prove the shape of the generated output and to
 * give the interface something real to render. See
 * `modeling/codegen_ts_ui_request.org`.
 */
const text = z.string().default('');

export const wireCountrySchema = z.object({
  version: z.int().nonnegative().default(0),
  tenant_id: text,
  image_id: z.string().nullable().default(null),
  coding_scheme_code: z.string().nullable().default(null),
  alpha2_code: text,
  alpha3_code: text,
  numeric_code: text,
  name: text,
  official_name: text,
  modified_by: text,
  change_reason_code: text,
  change_commentary: text,
  performed_by: text,
  recorded_at: text,
});

export type WireCountry = z.infer<typeof wireCountrySchema>;

/**
 * A page of countries.
 *
 * The list reply carries the total as well as the page, which is what lets the
 * footer say "1 of 4" rather than guessing from the page size.
 */
export const countryPageSchema = z.object({
  success: z.boolean().default(true),
  message: text,
  countries: z.array(wireCountrySchema).default([]),
  total_available_count: z.int().nonnegative().default(0),
});

export type WireCountryPage = z.infer<typeof countryPageSchema>;

/**
 * The request the list call takes.
 *
 * `as_of` has no default, because the C++ struct has none and `rfl::msgpack`
 * requires every member to be present. A field omitted here is a decode failure
 * on the service rather than a validation error here, which is a much harder
 * thing to see.
 */
export const listCountriesRequestSchema = z.object({
  offset: z.int().nonnegative(),
  limit: z.int().positive().max(1000),
  /** Empty means the current version. */
  as_of: z.string(),
});

/**
 * A save.
 *
 * The whole domain object, because the service replaces the record rather than
 * patching it. `version` is the optimistic lock: sending a stale one is refused,
 * so two people editing the same record is caught rather than one silently
 * overwriting the other.
 *
 * The audit metadata is stamped server-side, so the reason and the commentary
 * are the only audit fields the client supplies.
 */
export const saveCountryRequestSchema = z.object({
  data: wireCountrySchema,
});

export const saveCountryResponseSchema = z.object({
  success: z.boolean().default(false),
  message: text,
});

/**
 * A delete.
 *
 * By natural key, and plural, because the service deletes a set.
 */
export const deleteCountriesRequestSchema = z.object({
  alpha2_codes: z.array(z.string()),
});

export const deleteCountryResponseSchema = z.object({
  success: z.boolean().default(false),
  message: text,
});

/** The history of one record, newest version first as the service returns it. */
export const countryHistoryRequestSchema = z.object({
  alpha2_code: z.string(),
});

export const countryHistoryResponseSchema = z.object({
  history: z.array(wireCountrySchema).default([]),
  success: z.boolean().default(false),
  message: text,
});

/**
 * A country, as the interface works with it.
 *
 * camelCase, because this is no longer the wire. The mapping happens once, here,
 * so no screen has to know a snake_case field name.
 */
export interface Country {
  /** The alpha-2 code, which is this entity's identity. */
  readonly id: string;
  readonly version: number;
  readonly alpha2Code: string;
  readonly alpha3Code: string;
  readonly numericCode: string;
  readonly name: string;
  readonly officialName: string;
  readonly modifiedBy: string;
  readonly recordedAt: string;
  readonly changeReasonCode: string;
  readonly changeCommentary: string;
  readonly performedBy: string;
  /** The flag image, when the record carries one. */
  readonly imageId: string | null;
  /**
   * The record as the service sent it.
   *
   * Kept for the write path, because a save replaces the whole record and there
   * are fields the interface neither shows nor understands. Round-tripping them
   * untouched is the only way an amend cannot quietly drop one.
   */
  readonly wire: WireCountry;
}

export function mapCountry(row: WireCountry): Country {
  return {
    id: row.alpha2_code,
    version: row.version,
    alpha2Code: row.alpha2_code,
    alpha3Code: row.alpha3_code,
    numericCode: row.numeric_code,
    name: row.name,
    officialName: row.official_name,
    modifiedBy: row.modified_by,
    recordedAt: row.recorded_at,
    changeReasonCode: row.change_reason_code,
    changeCommentary: row.change_commentary,
    performedBy: row.performed_by,
    imageId: row.image_id,
    wire: row,
  };
}

/**
 * What a person can change, plus why.
 *
 * Deliberately not the whole record: the fields below are the editable ones, and
 * everything else is taken from the record being replaced.
 */
export interface CountryEdit {
  readonly alpha3Code: string;
  readonly numericCode: string;
  readonly name: string;
  readonly officialName: string;
  /** The version the person was looking at, for the optimistic lock. */
  readonly version: number;
  readonly changeReasonCode: string;
  readonly changeCommentary: string;
  /**
   * The image, or null for none.
   *
   * Optional because a form that does not offer an image must not clear one: a
   * save replaces the whole record, so silence here has to mean "unchanged"
   * rather than "none".
   */
  readonly imageId?: string | null;
}

/**
 * Merges an edit onto the record it came from.
 *
 * The identity is not editable, so it is carried through rather than accepted
 * from the form, and the audit actors and timestamp are the server's to stamp.
 */
export function applyEdit(current: WireCountry, edit: CountryEdit): WireCountry {
  return {
    ...current,
    // Only touched when the form said something about it.
    image_id: edit.imageId === undefined ? current.image_id : edit.imageId,
    alpha3_code: edit.alpha3Code,
    numeric_code: edit.numericCode,
    name: edit.name,
    official_name: edit.officialName,
    version: edit.version,
    change_reason_code: edit.changeReasonCode,
    change_commentary: edit.changeCommentary,
  };
}

/** A new record. The identity is the one field a person chooses. */
export function newCountry(input: {
  readonly alpha2Code: string;
  readonly alpha3Code: string;
  readonly numericCode: string;
  readonly name: string;
  readonly officialName: string;
  readonly changeReasonCode: string;
  readonly changeCommentary: string;
}): WireCountry {
  return {
    version: 0,
    tenant_id: '',
    image_id: null,
    coding_scheme_code: null,
    alpha2_code: input.alpha2Code,
    alpha3_code: input.alpha3Code,
    numeric_code: input.numericCode,
    name: input.name,
    official_name: input.officialName,
    modified_by: '',
    change_reason_code: input.changeReasonCode,
    change_commentary: input.changeCommentary,
    performed_by: '',
    recorded_at: '',
  };
}
