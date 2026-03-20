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
#include "ores.qt/BatchDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include "ui_BatchDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.compute/messaging/batch_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

BatchDetailDialog::BatchDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BatchDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

BatchDetailDialog::~BatchDetailDialog() {
    delete ui_;
}

QTabWidget* BatchDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* BatchDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* BatchDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void BatchDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BatchDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &BatchDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &BatchDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &BatchDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &BatchDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &BatchDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &BatchDetailDialog::onFieldChanged);
}

void BatchDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void BatchDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BatchDetailDialog::setBatch(
    const compute::domain::batch& batch) {
    batch_ = batch;
    updateUiFromBatch();
}

void BatchDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        batch_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void BatchDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BatchDetailDialog::updateUiFromBatch() {
    ui_->codeEdit->setText(QString::fromStdString(batch_.external_ref));
    ui_->nameEdit->setText(QString::fromStdString(batch_.status));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(batch_.status));

    populateProvenance(batch_.version,
                       batch_.modified_by,
                       batch_.performed_by,
                       batch_.recorded_at,
                       batch_.change_reason_code,
                       batch_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BatchDetailDialog::updateBatchFromUi() {
    if (createMode_) {
        batch_.external_ref = ui_->codeEdit->text().trimmed().toStdString();
    }
    batch_.status = ui_->nameEdit->text().trimmed().toStdString();
    batch_.status = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    batch_.modified_by = username_;
    batch_.performed_by = username_;
}

void BatchDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BatchDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BatchDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BatchDetailDialog::validateInput() {
    const QString external_ref_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !external_ref_val.isEmpty() && !name_val.isEmpty();
}

void BatchDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save compute batch while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateBatchFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving compute batch: " << batch_.external_ref;

    // Save not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Save operation is not yet implemented for this entity.");
}

void BatchDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete compute batch while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(batch_.external_ref);
    auto reply = MessageBoxHelper::question(this, "Delete Batch",
        QString("Are you sure you want to delete compute batch '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting compute batch: " << batch_.external_ref;

    // Delete not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Delete operation is not yet implemented for this entity.");
}

}
