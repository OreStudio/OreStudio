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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_WORKSPACE_CONTEXT_HPP
#define ORES_QT_WORKSPACE_CONTEXT_HPP

#include "ores.qt/export.hpp"
#include <QMetaType>
#include <QString>
#include <QVector>

namespace ores::qt {

/**
 * @brief Identifies the active workspace for a UI session.
 *
 * id == live_workspace_uuid (aaaaaaaa-...) is the production view.
 * Any other id represents a named workspace whose resolution_order
 * is applied when fetching data from the workspace service.
 *
 * @see ores_utility_live_workspace_id_fn() — SQL sentinel.
 * @see ores::utility::uuid::live_workspace_uuid_str — C++ constant.
 */
struct ORES_QT_API WorkspaceContext {
    inline static const QString live_workspace_id =
        QStringLiteral("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa");

    QString id = live_workspace_id;
    QString name = QStringLiteral("Live");
    QVector<QString> resolution_order = {live_workspace_id};

    bool is_live() const {
        return id == live_workspace_id;
    }
};

}

Q_DECLARE_METATYPE(ores::qt::WorkspaceContext)

#endif
