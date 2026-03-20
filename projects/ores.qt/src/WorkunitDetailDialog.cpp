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
#include "ores.qt/WorkunitDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include "ui_WorkunitDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.compute/messaging/workunit_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

WorkunitDetailDialog::WorkunitDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::WorkunitDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

WorkunitDetailDialog::~WorkunitDetailDialog() {
    delete ui_;
}

QTabWidget* WorkunitDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* WorkunitDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* WorkunitDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void WorkunitDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void WorkunitDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &WorkunitDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &WorkunitDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &WorkunitDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &WorkunitDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &WorkunitDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &WorkunitDetailDialog::onFieldChanged);
}

void WorkunitDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void WorkunitDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void WorkunitDetailDialog::setWorkunit(
    const compute::domain::workunit& workunit) {
    workunit_ = workunit;
    updateUiFromWorkunit();
}

void WorkunitDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        workunit_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkunitDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void WorkunitDetailDialog::updateUiFromWorkunit() {
    ui_->codeEdit->setText(QString::fromStdString(workunit_.input_uri));
    ui_->nameEdit->setText(QString::fromStdString(workunit_.input_uri));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(workunit_.config_uri));

    populateProvenance(workunit_.version,
                       workunit_.modified_by,
                       workunit_.performed_by,
                       workunit_.recorded_at,
                       workunit_.change_reason_code,
                       workunit_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkunitDetailDialog::updateWorkunitFromUi() {
    if (createMode_) {
        workunit_.input_uri = ui_->codeEdit->text().trimmed().toStdString();
    }
    workunit_.input_uri = ui_->nameEdit->text().trimmed().toStdString();
    workunit_.config_uri = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    workunit_.modified_by = username_;
    workunit_.performed_by = username_;
}

void WorkunitDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void WorkunitDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void WorkunitDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool WorkunitDetailDialog::validateInput() {
    const QString input_uri_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !input_uri_val.isEmpty() && !name_val.isEmpty();
}

void WorkunitDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save workunit while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateWorkunitFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving workunit: " << workunit_.input_uri;

    // Save not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Save operation is not yet implemented for this entity.");
}

void WorkunitDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete workunit while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(workunit_.input_uri);
    auto reply = MessageBoxHelper::question(this, "Delete Workunit",
        QString("Are you sure you want to delete workunit '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting workunit: " << workunit_.input_uri;

    // Delete not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Delete operation is not yet implemented for this entity.");
}

}
