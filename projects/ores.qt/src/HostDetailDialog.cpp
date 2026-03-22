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
#include "ores.qt/HostDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include "ui_HostDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.compute/messaging/host_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

HostDetailDialog::HostDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::HostDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

HostDetailDialog::~HostDetailDialog() {
    delete ui_;
}

QTabWidget* HostDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* HostDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* HostDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void HostDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void HostDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &HostDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &HostDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &HostDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &HostDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &HostDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &HostDetailDialog::onFieldChanged);
}

void HostDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void HostDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void HostDetailDialog::setHost(
    const compute::domain::host& host) {
    host_ = host;
    updateUiFromHost();
}

void HostDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        host_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void HostDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void HostDetailDialog::updateUiFromHost() {
    ui_->codeEdit->setText(QString::fromStdString(host_.external_id));
    ui_->nameEdit->setText(QString::fromStdString(host_.location));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(host_.gpu_type));

    populateProvenance(host_.version,
                       host_.modified_by,
                       host_.performed_by,
                       host_.recorded_at,
                       host_.change_reason_code,
                       host_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void HostDetailDialog::updateHostFromUi() {
    if (createMode_) {
        host_.external_id = ui_->codeEdit->text().trimmed().toStdString();
    }
    host_.location = ui_->nameEdit->text().trimmed().toStdString();
    host_.gpu_type = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    host_.modified_by = username_;
    host_.performed_by = username_;
}

void HostDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void HostDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void HostDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool HostDetailDialog::validateInput() {
    const QString external_id_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !external_id_val.isEmpty() && !name_val.isEmpty();
}

void HostDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save compute host while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateHostFromUi();

    const auto crOpType = createMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_,
        createMode_ ? "system" : "common");
    if (!crSel) return;
    host_.change_reason_code = crSel->reason_code;
    host_.change_commentary = crSel->commentary;

    BOOST_LOG_SEV(lg(), info) << "Saving compute host: " << host_.external_id;

    // Save not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Save operation is not yet implemented for this entity.");
}

void HostDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete compute host while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(host_.external_id);
    auto reply = MessageBoxHelper::question(this, "Delete Host",
        QString("Are you sure you want to delete compute host '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(
        ChangeReasonDialog::OperationType::Delete, true, "common");
    if (!crSel) return;

    BOOST_LOG_SEV(lg(), info) << "Deleting compute host: " << host_.external_id;

    // Delete not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Delete operation is not yet implemented for this entity.");
}

}
