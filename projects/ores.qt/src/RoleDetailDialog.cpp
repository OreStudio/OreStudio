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
#include "ores.qt/RoleDetailDialog.hpp"
#include "ores.qt/WidgetUtils.hpp"

#include <QListWidgetItem>
#include <algorithm>
#include "ui_RoleDetailDialog.h"

namespace ores::qt {

using namespace ores::logging;

RoleDetailDialog::RoleDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(std::make_unique<Ui::RoleDetailDialog>()) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    BOOST_LOG_SEV(lg(), debug) << "RoleDetailDialog created";
}

RoleDetailDialog::~RoleDetailDialog() {
    BOOST_LOG_SEV(lg(), debug) << "RoleDetailDialog destroyed";
}

QTabWidget* RoleDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* RoleDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* RoleDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void RoleDetailDialog::setRole(const iam::domain::role& role) {
    BOOST_LOG_SEV(lg(), debug) << "Setting role: " << role.name;

    currentRole_ = role;

    // Populate basic info
    ui_->nameEdit->setText(QString::fromStdString(role.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(role.description));

    // Populate provenance
    populateProvenance(role.version, role.modified_by, role.performed_by,
        role.recorded_at, "", "");

    // Populate permissions list
    populatePermissionsList();
}

void RoleDetailDialog::clearDialog() {
    BOOST_LOG_SEV(lg(), debug) << "Clearing dialog";

    currentRole_ = iam::domain::role{};

    ui_->nameEdit->clear();
    ui_->descriptionEdit->clear();
    clearProvenance();
    ui_->permissionsList->clear();
}

void RoleDetailDialog::populatePermissionsList() {
    ui_->permissionsList->clear();

    // Sort permissions alphabetically for display
    std::vector<std::string> sorted_permissions = currentRole_.permission_codes;
    std::ranges::sort(sorted_permissions);

    for (const auto& code : sorted_permissions) {
        auto* item = new QListWidgetItem(QString::fromStdString(code));

        // Add tooltip with description based on code pattern
        QString tooltip;
        if (code == "*") {
            tooltip = "Full access - all permissions";
        } else if (code.find(':') != std::string::npos) {
            const auto pos = code.find(':');
            const auto resource = code.substr(0, pos);
            const auto action = code.substr(pos + 1);
            tooltip = QString("Permission to %1 %2")
                .arg(QString::fromStdString(action))
                .arg(QString::fromStdString(resource));
        }

        if (!tooltip.isEmpty()) {
            item->setToolTip(tooltip);
        }

        ui_->permissionsList->addItem(item);
    }

    // Update group box title with count
    ui_->permissionsGroup->setTitle(
        QString("Assigned Permissions (%1)").arg(sorted_permissions.size()));
}

}
