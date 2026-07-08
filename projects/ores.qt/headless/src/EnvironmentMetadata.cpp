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
#include "ores.qt.headless/EnvironmentMetadata.hpp"
#include <QDir>
#include <QFileInfo>
#include <QProcess>
#include <QProcessEnvironment>

namespace ores::qt {

namespace {

/**
 * @brief Walk up from @p start looking for a =.git= entry (directory
 * for a normal clone, file for a worktree). Empty if none found within
 * a handful of levels.
 */
QString find_repo_root(const QString& start) {
    QDir dir(start);
    for (int i = 0; i < 10; ++i) {
        if (QFileInfo::exists(dir.filePath(".git")))
            return dir.absolutePath();
        if (!dir.cdUp())
            break;
    }
    return {};
}

QString run_git(const QString& repo_root, const QStringList& args) {
    QProcess process;
    process.setWorkingDirectory(repo_root);
    process.start("git", args);
    if (!process.waitForFinished(2000) || process.exitCode() != 0)
        return {};
    return QString::fromUtf8(process.readAllStandardOutput()).trimmed();
}

}

environment_metadata capture_environment_metadata(const QString& reference_path) {
    environment_metadata result;

    const QString start =
        reference_path.isEmpty() ? QDir::currentPath() : QFileInfo(reference_path).absolutePath();
    const QString repo_root = find_repo_root(start);
    if (!repo_root.isEmpty()) {
        result.branch = run_git(repo_root, {"rev-parse", "--abbrev-ref", "HEAD"});
        result.commit = run_git(repo_root, {"rev-parse", "--short", "HEAD"});
    }

    result.worktree = QProcessEnvironment::systemEnvironment().value("ORES_CHECKOUT_LABEL");

    return result;
}

}
