/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#ifndef ORES_QT_ENVIRONMENT_METADATA_HPP
#define ORES_QT_ENVIRONMENT_METADATA_HPP

#include "ores.qt/export.hpp"
#include <QString>

namespace ores::qt {

/**
 * @brief Branch/commit/worktree identity of the running checkout, read
 * live rather than hand-entered by the tester.
 */
struct environment_metadata final {
    QString branch;
    QString commit;
    QString worktree;
};

/**
 * @brief Capture the current environment metadata for the checkout
 * containing @p reference_path (walks up from there looking for
 * =.git=; falls back to the current working directory if
 * @p reference_path is empty).
 *
 * Branch/commit come from shelling out to =git= (best-effort: fields
 * are left empty, not an error, if =git= isn't on PATH or the
 * directory isn't a repo). Worktree identity comes from the
 * =ORES_CHECKOUT_LABEL= environment variable set by the shell
 * launcher (see =compass client start --instance-name=), empty if
 * unset.
 */
ORES_QT_API environment_metadata capture_environment_metadata(const QString& reference_path = {});

}

#endif
