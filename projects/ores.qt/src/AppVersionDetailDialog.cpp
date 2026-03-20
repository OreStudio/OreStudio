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
#include "ores.qt/AppVersionDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include "ui_AppVersionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.compute/messaging/app_version_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

AppVersionDetailDialog::AppVersionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::AppVersionDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

AppVersionDetailDialog::~AppVersionDetailDialog() {
    delete ui_;
}

QTabWidget* AppVersionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* AppVersionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* AppVersionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void AppVersionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void AppVersionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &AppVersionDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &AppVersionDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &AppVersionDetailDialog::onFieldChanged);
}

void AppVersionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void AppVersionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void AppVersionDetailDialog::setVersion(
    const compute::domain::app_version& app_version) {
    app_version_ = app_version;
    updateUiFromVersion();
}

void AppVersionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        app_version_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void AppVersionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void AppVersionDetailDialog::updateUiFromVersion() {
    ui_->codeEdit->setText(QString::fromStdString(app_version_.wrapper_version));
    ui_->nameEdit->setText(QString::fromStdString(app_version_.engine_version));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(app_version_.platform));

    populateProvenance(app_version_.version,
                       app_version_.modified_by,
                       app_version_.performed_by,
                       app_version_.recorded_at,
                       app_version_.change_reason_code,
                       app_version_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void AppVersionDetailDialog::updateVersionFromUi() {
    if (createMode_) {
        app_version_.wrapper_version = ui_->codeEdit->text().trimmed().toStdString();
    }
    app_version_.engine_version = ui_->nameEdit->text().trimmed().toStdString();
    app_version_.platform = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    app_version_.modified_by = username_;
    app_version_.performed_by = username_;
}

void AppVersionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void AppVersionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void AppVersionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool AppVersionDetailDialog::validateInput() {
    const QString wrapper_version_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !wrapper_version_val.isEmpty() && !name_val.isEmpty();
}

void AppVersionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save app version while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateVersionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving app version: " << app_version_.wrapper_version;

    // Save not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Save operation is not yet implemented for this entity.");
}

void AppVersionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete app version while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(app_version_.wrapper_version);
    auto reply = MessageBoxHelper::question(this, "Delete App Version",
        QString("Are you sure you want to delete app version '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting app version: " << app_version_.wrapper_version;

    // Delete not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Delete operation is not yet implemented for this entity.");
}

}
