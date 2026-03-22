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
#include "ores.qt/ResultDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include "ui_ResultDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ResultDetailDialog::ResultDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::ResultDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

ResultDetailDialog::~ResultDetailDialog() {
    delete ui_;
}

QTabWidget* ResultDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ResultDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ResultDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void ResultDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ResultDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &ResultDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &ResultDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &ResultDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &ResultDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &ResultDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &ResultDetailDialog::onFieldChanged);
}

void ResultDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ResultDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ResultDetailDialog::setResult(
    const compute::domain::result& result) {
    result_ = result;
    updateUiFromResult();
}

void ResultDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        result_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void ResultDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ResultDetailDialog::updateUiFromResult() {
    ui_->codeEdit->setText(QString::fromStdString(result_.modified_by));
    ui_->nameEdit->setText(QString::fromStdString(result_.output_uri));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(result_.modified_by));

    populateProvenance(result_.version,
                       result_.modified_by,
                       result_.performed_by,
                       result_.recorded_at,
                       result_.change_reason_code,
                       result_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ResultDetailDialog::updateResultFromUi() {
    if (createMode_) {
        result_.modified_by = ui_->codeEdit->text().trimmed().toStdString();
    }
    result_.output_uri = ui_->nameEdit->text().trimmed().toStdString();
    result_.modified_by = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    result_.modified_by = username_;
    result_.performed_by = username_;
}

void ResultDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ResultDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ResultDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ResultDetailDialog::validateInput() {
    const QString modified_by_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !modified_by_val.isEmpty() && !name_val.isEmpty();
}

void ResultDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save compute result while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateResultFromUi();

    const auto crOpType = createMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_,
        createMode_ ? "system" : "common");
    if (!crSel) return;
    result_.change_reason_code = crSel->reason_code;
    result_.change_commentary = crSel->commentary;

    BOOST_LOG_SEV(lg(), info) << "Saving compute result: " << result_.modified_by;

    // Save not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Save operation is not yet implemented for this entity.");
}

void ResultDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete compute result while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(result_.modified_by);
    auto reply = MessageBoxHelper::question(this, "Delete Result",
        QString("Are you sure you want to delete compute result '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(
        ChangeReasonDialog::OperationType::Delete, true, "common");
    if (!crSel) return;

    BOOST_LOG_SEV(lg(), info) << "Deleting compute result: " << result_.modified_by;

    // Delete not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Delete operation is not yet implemented for this entity.");
}

}
