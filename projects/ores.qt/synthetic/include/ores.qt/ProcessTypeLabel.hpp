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
#ifndef ORES_QT_PROCESS_TYPE_LABEL_HPP
#define ORES_QT_PROCESS_TYPE_LABEL_HPP

#include <QCoreApplication>
#include <QString>
#include <string>

namespace ores::qt {

/**
 * @brief Human-readable label for a synthetic price-process engine code.
 *
 * Mirrors the engine set in FxSpotRateEditor's kEngines (the editor combo's
 * source of truth) — kept here as a small, reusable lookup so summary/list
 * views (e.g. ClientFxSpotGenerationConfigModel) can show a friendly name
 * without depending on the editor.
 */
inline QString processTypeLabel(const std::string& processType) {
    if (processType == "geometric")
        return QCoreApplication::translate("ProcessTypeLabel", "Geometric Brownian Motion");
    if (processType == "arithmetic")
        return QCoreApplication::translate("ProcessTypeLabel", "Arithmetic Brownian Motion");
    if (processType == "ou")
        return QCoreApplication::translate("ProcessTypeLabel", "Ornstein-Uhlenbeck");
    return QString::fromStdString(processType); // unknown code: show it verbatim
}

}

#endif
