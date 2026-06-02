/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_ABSTRACT_CLIENT_MODEL_HPP
#define ORES_QT_ABSTRACT_CLIENT_MODEL_HPP

#include <QString>
#include <QAbstractTableModel>
#include "ores.qt/WorkspaceContext.hpp"
#include "ores.qt/export.hpp"

namespace ores::qt {

/**
 * @brief Base class for all client-side entity models.
 *
 * Declares the standard loading lifecycle signals that EntityListMdiWindow
 * connects to in order to manage the loading indicator and reload button
 * state automatically. Subclasses must emit these signals when an async
 * fetch completes or fails.
 */
class ORES_QT_API AbstractClientModel : public QAbstractTableModel {
    Q_OBJECT

public:
    using QAbstractTableModel::QAbstractTableModel;

    /**
     * @brief Set the workspace context for this window's data requests.
     *
     * When set to anything other than the Live sentinel, all fetch operations
     * will use this workspace id in X-Workspace-Id instead of the shared
     * ClientManager workspace context. Call before or instead of reload().
     */
    void setWorkspaceContext(const WorkspaceContext& ctx) { workspace_ctx_ = ctx; }

    /**
     * @brief Get the current per-window workspace context.
     */
    const WorkspaceContext& workspaceContext() const { return workspace_ctx_; }

signals:
    void dataLoaded();
    void loadError(const QString& error_message, const QString& details = {});

private:
    WorkspaceContext workspace_ctx_;
};

}

#endif
