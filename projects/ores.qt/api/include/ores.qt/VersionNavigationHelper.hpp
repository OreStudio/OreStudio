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
#ifndef ORES_QT_VERSION_NAVIGATION_HELPER_HPP
#define ORES_QT_VERSION_NAVIGATION_HELPER_HPP

#include <QObject>

namespace ores::qt {

/**
 * @brief Wires a just-opened version DetailDialog to the full version list
 * held by the HistoryDialog that requested it, so the dialog's
 * first/prev/next/last navigation has something to navigate across.
 *
 * A detail-dialog-for-a-version is normally opened by
 * `HistoryDialog::openVersionRequested(entity, versionNumber)`, which only
 * carries a single version's data. `sender` is a Controller's own signal
 * sender at that point in the slot — the HistoryDialog instance that emitted
 * it. When it is an @p HistoryDialogT with a non-empty history, this pulls
 * that history via @p HistoryDialogT::getHistory() and calls
 * @p detailDialog->setHistory(history, versionNumber), enabling navigation.
 *
 * @return true if history was wired (caller should skip its single-version
 * fallback path), false if the sender wasn't the expected history dialog
 * type or its history was empty (caller should fall back to a plain
 * read-only single-version display).
 */
template <typename HistoryDialogT, typename DetailDialogT>
bool wireVersionHistory(QObject* sender, DetailDialogT* detailDialog, int versionNumber) {
    auto* historyDialog = qobject_cast<HistoryDialogT*>(sender);
    if (!historyDialog || historyDialog->getHistory().versions.empty())
        return false;
    detailDialog->setHistory(historyDialog->getHistory(), versionNumber);
    return true;
}

}

#endif
