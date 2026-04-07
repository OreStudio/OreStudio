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
#include "ores.qt/BadgeDefinitionDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include "ui_BadgeDefinitionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq.api/messaging/badge_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

BadgeDefinitionDetailDialog::BadgeDefinitionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BadgeDefinitionDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

BadgeDefinitionDetailDialog::~BadgeDefinitionDetailDialog() {
    delete ui_;
}

QTabWidget* BadgeDefinitionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* BadgeDefinitionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* BadgeDefinitionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void BadgeDefinitionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BadgeDefinitionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &BadgeDefinitionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &BadgeDefinitionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &BadgeDefinitionDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &BadgeDefinitionDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &BadgeDefinitionDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &BadgeDefinitionDetailDialog::onFieldChanged);
}

void BadgeDefinitionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void BadgeDefinitionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BadgeDefinitionDetailDialog::setDefinition(
    const dq::domain::badge_definition& definition) {
    definition_ = definition;
    updateUiFromDefinition();
}

void BadgeDefinitionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void BadgeDefinitionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BadgeDefinitionDetailDialog::updateUiFromDefinition() {
    ui_->codeEdit->setText(QString::fromStdString(definition_.code));
    ui_->nameEdit->setText(QString::fromStdString(definition_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(definition_.description));

    populateProvenance(definition_.version,
                       definition_.modified_by,
                       definition_.performed_by,
                       definition_.recorded_at,
                       definition_.change_reason_code,
                       definition_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BadgeDefinitionDetailDialog::updateDefinitionFromUi() {
    if (createMode_) {
        definition_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    definition_.name = ui_->nameEdit->text().trimmed().toStdString();
    definition_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    definition_.modified_by = username_;
    definition_.performed_by = username_;
}

void BadgeDefinitionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BadgeDefinitionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BadgeDefinitionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BadgeDefinitionDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !code_val.isEmpty() && !name_val.isEmpty();
}

void BadgeDefinitionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save badge definition while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateDefinitionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving badge definition: " << definition_.code;

    QPointer<BadgeDefinitionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, definition = definition_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::save_badge_definition_request request;
        request.data = definition;
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, response_result.error()};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Badge Definition saved successfully";
            QString code = QString::fromStdString(self->definition_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->definitionSaved(code);
            self->notifySaveSuccess(tr("Badge Definition '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void BadgeDefinitionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete badge definition while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(definition_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Badge Definition",
        QString("Are you sure you want to delete badge definition '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting badge definition: " << definition_.code;

    QPointer<BadgeDefinitionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code_str = definition_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::delete_badge_definition_request request;
        request.codes = {code_str};
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, response_result.error()};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Badge Definition deleted successfully";
            emit self->statusMessage(QString("Badge Definition '%1' deleted").arg(code));
            emit self->definitionDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
