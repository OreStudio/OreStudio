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
#include "ores.qt/WorkspaceDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ui_WorkspaceDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.workspace.api/messaging/workspace_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

WorkspaceDetailDialog::WorkspaceDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::WorkspaceDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

WorkspaceDetailDialog::~WorkspaceDetailDialog() {
    delete ui_;
}

QTabWidget* WorkspaceDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* WorkspaceDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* WorkspaceDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void WorkspaceDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Archive, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    // ID is always auto-generated — hide the id field entirely
    ui_->labelId->setVisible(false);
    ui_->codeEdit->setVisible(false);
}

void WorkspaceDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &WorkspaceDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &WorkspaceDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &WorkspaceDetailDialog::onCloseClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &WorkspaceDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &WorkspaceDetailDialog::onFieldChanged);
}

void WorkspaceDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void WorkspaceDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void WorkspaceDetailDialog::setWorkspace(
    const workspace::domain::workspace& workspace) {
    workspace_ = workspace;
    updateUiFromWorkspace();
}

void WorkspaceDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(false);
    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkspaceDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void WorkspaceDetailDialog::updateUiFromWorkspace() {
    ui_->nameEdit->setText(QString::fromStdString(workspace_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(workspace_.description));

    if (workspace_.parent_workspace_id) {
        ui_->inheritsFromEdit->setText(QString::fromStdString(
            boost::uuids::to_string(*workspace_.parent_workspace_id)));
    } else {
        ui_->inheritsFromEdit->setText(QString());
    }

    if (workspace_.version > 0 && provenanceWidget()) {
        provenanceWidget()->populate(
            workspace_.version,
            workspace_.modified_by,
            workspace_.performed_by,
            workspace_.recorded_at,
            workspace_.change_reason_code,
            workspace_.change_commentary);
        setProvenanceEnabled(true);
    } else {
        clearProvenance();
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkspaceDetailDialog::updateWorkspaceFromUi() {
    workspace_.name = ui_->nameEdit->text().trimmed().toStdString();
    workspace_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    workspace_.modified_by = username_;
    if (workspace_.change_reason_code.empty())
        workspace_.change_reason_code = "system.new_record";
}

void WorkspaceDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void WorkspaceDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void WorkspaceDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool WorkspaceDetailDialog::validateInput() {
    return !ui_->nameEdit->text().trimmed().isEmpty();
}

void WorkspaceDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save workspace while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateWorkspaceFromUi();

    BOOST_LOG_SEV(lg(), info) << "Creating workspace: " << workspace_.name;

    QPointer<WorkspaceDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
        std::string id;
    };

    auto task = [self, workspace = workspace_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        workspace::messaging::create_workspace_request request;
        request.data = workspace;
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message, response_result->id};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Workspace created with id: " << result.id;
            QString code = QString::fromStdString(result.id);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->workspaceSaved(code);
            self->notifySaveSuccess(tr("Workspace '%1' created").arg(code));
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

void WorkspaceDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot archive workspace while disconnected from server.");
        return;
    }

    QString name = QString::fromStdString(workspace_.name);
    auto reply = MessageBoxHelper::question(this, "Archive Workspace",
        QString("Are you sure you want to archive workspace '%1'?").arg(name),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Archiving workspace id: "
                              << boost::uuids::to_string(workspace_.id);

    QPointer<WorkspaceDetailDialog> self = this;

    struct ArchiveResult {
        bool success;
        std::string message;
    };

    const std::string ws_id = boost::uuids::to_string(workspace_.id);
    const std::string actor = username_;
    auto task = [self, ws_id, actor]() -> ArchiveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        workspace::messaging::archive_workspace_request request;
        request.id = ws_id;
        request.modified_by = actor;
        request.change_reason_code = "user.archive";
        request.change_commentary = "Archived by user";
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<ArchiveResult>(self);
    connect(watcher, &QFutureWatcher<ArchiveResult>::finished,
            self, [self, name, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Workspace archived successfully";
            emit self->statusMessage(
                QString("Workspace '%1' archived").arg(name));
            emit self->workspaceDeleted(name);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Archive failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Archive Failed", errorMsg);
        }
    });

    QFuture<ArchiveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
