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
 */

/**
 * What a generated entity declaration may say, and nothing more.
 *
 * One shared DataTable and one shared FieldControl render every entity, and
 * the per-entity files under src/<component>/ui/ are their data. The types
 * live here so a generated file is checked against the contract rather than
 * merely resembling it, and so a member nothing consumes cannot be added to
 * one entity alone.
 *
 * The generated files import ColumnMeta and FieldMeta type-only, which
 * disappears at compile time: they carry data and no runtime dependency.
 */

/** How a cell is rendered. The set is closed and matches the C++ column styles. */
export type ColumnStyle =
  | 'text_left'
  | 'text_center'
  | 'mono_left'
  | 'mono_center'
  | 'mono_bold_left'
  | 'mono_right'
  | 'mono_bold_center'
  | 'icon_centered'
  | 'icon_text_left'
  | 'badge_centered';

/** How a field is edited. The set is closed and matches the model's widget types. */
export type FieldControl =
  | 'line_edit'
  | 'text_edit'
  | 'static_combo'
  | 'dynamic_combo'
  | 'flagged_combo'
  | 'check_box'
  | 'spin_box'
  | 'colour'
  | 'date';

export interface ColumnMeta {
  /** The wire field name, which is the key the row object holds. */
  readonly name: string;
  /** Translation key for the header. */
  readonly headerKey: string;
  readonly style: ColumnStyle;
  /** Hidden by default, available through the column menu. */
  readonly hidden: boolean;
  /** A width hint in pixels. The user's own resize wins. */
  readonly width?: number;
  /**
   * The code domain a pill resolves against, for `badge_centered`. The shared
   * table looks the display text and colour up rather than the model carrying
   * them, so a rebranded badge changes in one place.
   */
  readonly codeDomain?: string;
  /** True when the cell renders a flag. */
  readonly flag?: boolean;
  /** True when the value is a timestamp, so the cell renders it relatively. */
  readonly temporal?: boolean;
}

export interface FieldOption {
  /** The stored value. */
  readonly value: string;
  /** Translation key for what the person sees. */
  readonly labelKey: string;
}

/** Where a foreign key's options come from. */
export interface LookupSource {
  /** The collection to fetch. */
  readonly collection: string;
  /** The field holding the stored value. */
  readonly valueField: string;
  /** The field holding the display text. */
  readonly labelField: string;
}

export interface FieldMeta {
  /** The wire field name. */
  readonly name: string;
  /** Translation key for the label. */
  readonly labelKey: string;
  readonly control: FieldControl;
  /** A value must be supplied. */
  readonly required: boolean;
  /** The natural key. */
  readonly isKey: boolean;
  /** The field may hold no value. */
  readonly nullable: boolean;
  /** Translation key for the placeholder, when the model gives one. */
  readonly placeholderKey?: string;
  /** Translation key for a hint below the control. */
  readonly hintKey?: string;
  /** Editable when creating, read-only afterwards. */
  readonly readOnlyAfterCreate?: boolean;
  /** For `check_box`: allow a "not set" state distinct from false. */
  readonly triState?: boolean;
  /** For `static_combo`: the declared options. */
  readonly options?: readonly FieldOption[];
  /** For `dynamic_combo` and `flagged_combo`: where the options come from. */
  readonly lookup?: LookupSource;
  /** For `spin_box`. */
  readonly min?: number;
  readonly max?: number;
  /** For a field whose value is a code resolved to a pill or a flag. */
  readonly codeDomain?: string;
  /** For a text field with a known limit. */
  readonly maxLength?: number;
}

/**
 * One tab of a detail screen.
 *
 * Not generated: which fields belong together is a domain judgement the model
 * does not carry. This is the shape of the hand-written overlay.
 */
export interface FieldGroup {
  readonly id: string;
  /** Translation key for the tab's label. */
  readonly titleKey: string;
  /** Field names, in the order they appear within the group. */
  readonly fields: readonly string[];
}

/**
 * The image an entity carries, when it carries one.
 *
 * Emitted from the model's own flag declaration, because an image is not a
 * field: no form control edits it, and the shared screen renders a picker
 * beside the fields rather than among them. What is generated is where the
 * image lives and what kind it is; what it looks like when chosen is the
 * screen's business.
 */
export interface EntityImage {
  /** The wire field holding the image's identifier, e.g. `image_id`. */
  readonly field: string;
  /**
   * What the images are.
   *
   * A country's image is its flag and a party's is its logo, and a picker
   * that offered both would offer the wrong ones. The kind is the model's
   * statement; the words a picker narrows by are the screen's.
   */
  readonly kind: 'flag' | 'image';
}

/** The per-entity bundle, so the registry has one thing to hold. */
export interface EntityMeta {
  readonly entity: string;
  readonly collection: string;
  /** The field a person recognises a record by. */
  readonly displayField: string;
  /** The natural key. */
  readonly keyField: string;
  readonly columns: readonly ColumnMeta[];
  readonly fields: readonly FieldMeta[];
  /** Present exactly when the entity owns an image the screen can change. */
  readonly image?: EntityImage;
}
