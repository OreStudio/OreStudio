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
#ifndef ORES_QT_REPO_FILE_FINDER_HPP
#define ORES_QT_REPO_FILE_FINDER_HPP

#include "ores.qt.headless/export.hpp"
#include <QString>

namespace ores::qt {

/**
 * @brief Walk up from @p referencePath looking for @p filename at the
 * repo root (e.g. =.org-roam.db=, =compass.sh=). Empty if not found
 * within a handful of levels.
 *
 * Shared by anything that needs to locate a repo-root file from an
 * arbitrary doc/scenario path — e.g. =QaValidationRunnerWidget= and
 * =OrgDocViewerWindow= both need the org-roam index this way.
 */
ORES_QT_HEADLESS_API QString find_repo_file(const QString& referencePath, const QString& filename);

}

#endif
