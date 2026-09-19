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

import type { CSSProperties, ReactNode } from 'react';
import { cx } from '../Primitives.js';
import { ICONS, type IconName } from './index.js';

/**
 * An icon, coloured by the text around it.
 *
 * Fluent icons ship with a hardcoded near-black fill, so dropping one in as an
 * `<img>` on this dark interface renders it invisible. Laid over a mask and
 * painted with `currentColor` instead, an icon takes the colour of whatever it
 * sits in, which is what the design needs: the same icon is muted in a sidebar
 * and full-strength in a toolbar.
 *
 * `background-color: currentColor` with `mask-image` is the standard way to do
 * this without editing every vendored file.
 */
export function MaskIcon({
  name,
  className,
  style,
}: {
  readonly name: IconName;
  readonly className?: string;
  readonly style?: CSSProperties;
}): ReactNode {
  return (
    <span
      aria-hidden
      className={cx('inline-block shrink-0 bg-current', className)}
      style={{
        maskImage: `url("${ICONS[name]}")`,
        WebkitMaskImage: `url("${ICONS[name]}")`,
        maskSize: 'contain',
        WebkitMaskSize: 'contain',
        maskRepeat: 'no-repeat',
        WebkitMaskRepeat: 'no-repeat',
        maskPosition: 'center',
        WebkitMaskPosition: 'center',
        ...style,
      }}
    />
  );
}
