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
#include "ores.qt/PartyPickerDialog.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

PartyPickerDialog::PartyPickerDialog(
    const std::vector<PartyInfo>& parties,
    ClientManager* clientManager,
    QWidget* parent)
    : QDialog(parent),
      listWidget_(new QListWidget(this)),
      okButton_(new QPushButton("Select", this)),
      cancelButton_(new QPushButton("Cancel", this)),
      clientManager_(clientManager),
      parties_(parties) {

    setupUi();

    connect(okButton_, &QPushButton::clicked, this, &PartyPickerDialog::onOkClicked);
    connect(cancelButton_, &QPushButton::clicked, this, &QDialog::reject);
    connect(listWidget_, &QListWidget::itemDoubleClicked, this, &PartyPickerDialog::onOkClicked);
    connect(listWidget_, &QListWidget::itemSelectionChanged, this, [this]() {
        okButton_->setEnabled(!listWidget_->selectedItems().isEmpty());
    });
}

boost::uuids::uuid PartyPickerDialog::selectedPartyId() const {
    const auto row = listWidget_->currentRow();
    if (row < 0 || row >= static_cast<int>(parties_.size())) {
        return boost::uuids::uuid{};
    }
    return parties_[static_cast<std::size_t>(row)].id;
}

QString PartyPickerDialog::selectedPartyName() const {
    const auto row = listWidget_->currentRow();
    if (row < 0 || row >= static_cast<int>(parties_.size())) {
        return {};
    }
    return parties_[static_cast<std::size_t>(row)].name;
}

void PartyPickerDialog::setupUi() {
    setWindowTitle("Select Party");
    setModal(true);
    setMinimumWidth(360);
    setFixedWidth(360);
    setSizeGripEnabled(false);

    auto* info_label = new QLabel(
        "Your account is associated with multiple parties.\n"
        "Please select the party context for this session.", this);
    info_label->setWordWrap(true);
    info_label->setStyleSheet("QLabel { margin-bottom: 8px; }");

    // Populate list with party names
    for (const auto& p : parties_) {
        listWidget_->addItem(p.name);
    }

    // Auto-select first item
    if (!parties_.empty()) {
        listWidget_->setCurrentRow(0);
    }

    // Only enable OK when something is selected
    okButton_->setEnabled(!parties_.empty());
    okButton_->setDefault(true);
    okButton_->setIcon(IconUtils::createRecoloredIcon(Icon::Checkmark, IconUtils::DefaultIconColor));
    cancelButton_->setIcon(IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    auto* button_layout = new QHBoxLayout();
    button_layout->addStretch();
    button_layout->addWidget(cancelButton_);
    button_layout->addWidget(okButton_);

    auto* main_layout = new QVBoxLayout(this);
    main_layout->addWidget(info_label);
    main_layout->addWidget(listWidget_);
    main_layout->addSpacing(8);
    main_layout->addLayout(button_layout);

    listWidget_->setFocus();
}

void PartyPickerDialog::onOkClicked() {
    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    const auto id = selectedPartyId();
    const auto name = selectedPartyName();

    if (id.is_nil()) {
        MessageBoxHelper::warning(this, "No Selection", "Please select a party.");
        return;
    }

    if (!clientManager_->selectParty(id, name)) {
        MessageBoxHelper::critical(this, "Party Selection Failed",
            "The server rejected the party selection. Please try again.");
        return;
    }

    accept();
}

}
