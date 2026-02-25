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
#include "ores.qt/DetailDialogBase.hpp"

#include <QMessageBox>
#include <QTabWidget>
#include "ores.qt/ProvenanceWidget.hpp"

namespace ores::qt {

// MOC requires a non-inline destructor for proper vtable generation
DetailDialogBase::~DetailDialogBase() {}

void DetailDialogBase::setProvenanceEnabled(bool enabled) {
    auto* tw = tabWidget();
    auto* pt = provenanceTab();
    if (!tw || !pt) return;

    for (int i = 0; i < tw->count(); ++i) {
        if (tw->widget(i) == pt) {
            tw->setTabEnabled(i, enabled);
            return;
        }
    }
}

void DetailDialogBase::populateProvenance(
        int version,
        const std::string& modified_by,
        const std::string& performed_by,
        std::chrono::system_clock::time_point recorded_at,
        const std::string& change_reason_code,
        const std::string& change_commentary) {
    auto* pw = provenanceWidget();
    if (!pw) return;
    pw->populate(version, modified_by, performed_by, recorded_at,
                 change_reason_code, change_commentary);
}

void DetailDialogBase::clearProvenance() {
    auto* pw = provenanceWidget();
    if (!pw) return;
    pw->clear();
}

void DetailDialogBase::onCloseClicked() {
    if (hasUnsavedChanges()) {
        auto reply = QMessageBox::question(
            this, tr("Unsaved Changes"),
            tr("You have unsaved changes. Close anyway?"),
            QMessageBox::Yes | QMessageBox::No);
        if (reply != QMessageBox::Yes)
            return;
    }
    requestClose();
}

}
