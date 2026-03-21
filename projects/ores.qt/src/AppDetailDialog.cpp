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
#include "ores.qt/AppDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include "ui_AppDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.compute/messaging/app_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

AppDetailDialog::AppDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::AppDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

AppDetailDialog::~AppDetailDialog() {
    delete ui_;
}

QTabWidget* AppDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* AppDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* AppDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void AppDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void AppDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &AppDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &AppDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &AppDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &AppDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &AppDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &AppDetailDialog::onFieldChanged);
}

void AppDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void AppDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void AppDetailDialog::setApp(
    const compute::domain::app& app) {
    app_ = app;
    updateUiFromApp();
}

void AppDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        app_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void AppDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void AppDetailDialog::updateUiFromApp() {
    ui_->codeEdit->setText(QString::fromStdString(app_.name));
    ui_->nameEdit->setText(QString::fromStdString(app_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(app_.description));

    populateProvenance(app_.version,
                       app_.modified_by,
                       app_.performed_by,
                       app_.recorded_at,
                       app_.change_reason_code,
                       app_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void AppDetailDialog::updateAppFromUi() {
    if (createMode_) {
        app_.name = ui_->codeEdit->text().trimmed().toStdString();
    }
    app_.name = ui_->nameEdit->text().trimmed().toStdString();
    app_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    app_.modified_by = username_;
    app_.performed_by = username_;
}

void AppDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void AppDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void AppDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool AppDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !code_val.isEmpty() && !name_val.isEmpty();
}

void AppDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save compute app while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateAppFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving compute app: " << app_.name;

    using FutureResult = std::pair<bool, std::string>;
    QPointer<AppDetailDialog> self = this;
    const compute::domain::app appToSave = app_;

    QFuture<FutureResult> future =
        QtConcurrent::run([self, appToSave]() -> FutureResult {
            if (!self) return {false, ""};

            compute::messaging::save_app_request request;
            request.app = appToSave;

            auto result =
                self->clientManager_->process_authenticated_request(
                    std::move(request));

            if (!result) return {false, "Failed to communicate with server"};
            return {result->success, result->message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(this);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, appToSave]() {
        if (!self) return;
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->appSaved(QString::fromStdString(appToSave.name));
            self->notifySaveSuccess(
                tr("App '%1' saved").arg(QString::fromStdString(appToSave.name)));
        } else {
            BOOST_LOG_SEV(lg(), error) << "App save failed: " << message;
            emit self->errorMessage(
                QString("Failed to save app: %1").arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(message));
        }
    });
    watcher->setFuture(future);
}

void AppDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete compute app while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(app_.name);
    auto reply = MessageBoxHelper::question(this, "Delete App",
        QString("Are you sure you want to delete compute app '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting compute app: " << app_.name;

    // Delete not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Delete operation is not yet implemented for this entity.");
}

}
