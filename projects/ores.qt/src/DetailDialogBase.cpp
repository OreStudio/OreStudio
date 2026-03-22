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
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.dq/domain/change_reason.hpp"

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

std::optional<DetailDialogBase::change_reason_selection>
DetailDialogBase::promptChangeReason(ChangeReasonDialog::OperationType opType,
                                     bool isDirty,
                                     std::string_view category) {
    if (!changeReasonCache_ || !changeReasonCache_->isLoaded()) {
        emit errorMessage(tr("Change reasons not loaded. Please try again."));
        return std::nullopt;
    }

    const auto cat = std::string{category};
    std::vector<dq::domain::change_reason> reasons;
    switch (opType) {
        case ChangeReasonDialog::OperationType::Create:
            reasons = changeReasonCache_->getReasonsForNew(cat);
            break;
        case ChangeReasonDialog::OperationType::Amend:
            reasons = changeReasonCache_->getReasonsForAmend(cat);
            break;
        case ChangeReasonDialog::OperationType::Delete:
            reasons = changeReasonCache_->getReasonsForDelete(cat);
            break;
    }

    if (reasons.empty()) {
        emit errorMessage(
            tr("No change reasons available. Please contact administrator."));
        return std::nullopt;
    }

    ChangeReasonDialog dlg(reasons, opType, isDirty, this);
    if (dlg.exec() != QDialog::Accepted)
        return std::nullopt;

    return change_reason_selection{dlg.selectedReasonCode(), dlg.commentary()};
}

void DetailDialogBase::onCloseClicked() {
    if (hasUnsavedChanges()) {
        auto reply = MessageBoxHelper::question(
            this, tr("Unsaved Changes"),
            tr("You have unsaved changes. Close anyway?"),
            QMessageBox::Yes | QMessageBox::No);
        if (reply != QMessageBox::Yes)
            return;
    }
    closeConfirmed_ = true;
    requestClose();
}

}
