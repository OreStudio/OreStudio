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
#include <QComboBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
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
      clientManager_(nullptr),
      parentWatcher_(new QFutureWatcher<WorkspaceListResult>(this)) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();

    connect(parentWatcher_, &QFutureWatcher<WorkspaceListResult>::finished,
            this, &WorkspaceDetailDialog::onParentWorkspacesLoaded);
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
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void WorkspaceDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &WorkspaceDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &WorkspaceDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &WorkspaceDetailDialog::onCloseClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &WorkspaceDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &WorkspaceDetailDialog::onFieldChanged);
    connect(ui_->sourcePathEdit, &QLineEdit::textChanged, this,
            &WorkspaceDetailDialog::onFieldChanged);
    connect(ui_->parentCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &WorkspaceDetailDialog::onFieldChanged);
    connect(ui_->ownerEdit, &QLineEdit::textChanged, this,
            &WorkspaceDetailDialog::onFieldChanged);
    connect(ui_->statusEdit, &QLineEdit::textChanged, this,
            &WorkspaceDetailDialog::onFieldChanged);
}

void WorkspaceDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    if (clientManager_ && clientManager_->isConnected())
        loadParentWorkspaces();
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
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        workspace_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkspaceDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->sourcePathEdit->setReadOnly(readOnly);
    ui_->parentCombo->setEnabled(!readOnly);
    ui_->ownerEdit->setReadOnly(readOnly);
    ui_->statusEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void WorkspaceDetailDialog::updateUiFromWorkspace() {
    ui_->nameEdit->setText(QString::fromStdString(workspace_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(workspace_.description));
    ui_->sourcePathEdit->setText(QString::fromStdString(workspace_.source_path));
    selectCurrentParent();
    ui_->ownerEdit->setText(QString::fromStdString(
        boost::uuids::to_string(workspace_.owner_id)));
    ui_->statusEdit->setText(QString::fromStdString(workspace_.status_code));

    populateProvenance(workspace_.version,
                       workspace_.modified_by,
                       workspace_.performed_by,
                       workspace_.recorded_at,
                       workspace_.change_reason_code,
                       workspace_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkspaceDetailDialog::updateWorkspaceFromUi() {
    if (createMode_) {
        workspace_.name = ui_->nameEdit->text().trimmed().toStdString();
    }
    workspace_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    workspace_.source_path = ui_->sourcePathEdit->text().trimmed().toStdString();
    workspace_.status_code = ui_->statusEdit->text().trimmed().toStdString();
    workspace_.modified_by = username_;

    const QString parentUuid = ui_->parentCombo->currentData().toString();
    if (parentUuid.isEmpty()) {
        workspace_.parent_workspace_id = std::nullopt;
    } else {
        try {
            boost::uuids::string_generator gen;
            workspace_.parent_workspace_id = gen(parentUuid.toStdString());
        } catch (...) {
            workspace_.parent_workspace_id = std::nullopt;
        }
    }
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
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true
        && !name_val.isEmpty()
    ;
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

    BOOST_LOG_SEV(lg(), info) << "Saving workspace: "
        << workspace_.name;

    QPointer<WorkspaceDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
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

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Workspace saved successfully";
            QString code = QString::fromStdString(
                self->workspace_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->workspaceSaved(code);
            self->notifySaveSuccess(tr("Workspace '%1' saved").arg(code));
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
            "Cannot delete workspace while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(
        workspace_.name);
    auto reply = MessageBoxHelper::question(this, "Delete Workspace",
        QString("Are you sure you want to delete workspace '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting workspace: "
        << workspace_.name;

    QPointer<WorkspaceDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(workspace_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        workspace::messaging::archive_workspace_request request;
        request.id = id_str;
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Workspace deleted successfully";
            emit self->statusMessage(
                QString("Workspace '%1' deleted").arg(code));
            emit self->workspaceDeleted(code);
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

void WorkspaceDetailDialog::loadParentWorkspaces() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<WorkspaceDetailDialog> self = this;
    QFuture<WorkspaceListResult> future =
        QtConcurrent::run([self]() -> WorkspaceListResult {
            if (!self || !self->clientManager_)
                return {false, {}, "Dialog closed"};

            workspace::messaging::list_workspaces_request request;
            auto result = self->clientManager_->process_authenticated_request(
                std::move(request));

            if (!result)
                return {false, {}, QString::fromStdString(result.error())};

            return {true, std::move(result->workspaces), {}};
        });

    parentWatcher_->setFuture(future);
}

void WorkspaceDetailDialog::onParentWorkspacesLoaded() {
    const auto result = parentWatcher_->result();
    if (!result.success)
        return;

    const QString selfUuid = QString::fromStdString(
        boost::uuids::to_string(workspace_.id));

    const bool hadSignals = ui_->parentCombo->blockSignals(true);
    ui_->parentCombo->clear();
    ui_->parentCombo->addItem(tr("(none)"), QString{});

    for (const auto& ws : result.workspaces) {
        const QString uuid = QString::fromStdString(boost::uuids::to_string(ws.id));
        if (uuid == selfUuid)
            continue;
        const QString label = QString("%1 [%2]")
            .arg(QString::fromStdString(ws.name))
            .arg(uuid);
        ui_->parentCombo->addItem(label, uuid);
    }

    selectCurrentParent();
    ui_->parentCombo->blockSignals(hadSignals);
}

void WorkspaceDetailDialog::selectCurrentParent() {
    if (!workspace_.parent_workspace_id.has_value()) {
        ui_->parentCombo->setCurrentIndex(0);
        return;
    }

    const QString targetUuid = QString::fromStdString(
        boost::uuids::to_string(*workspace_.parent_workspace_id));

    for (int i = 0; i < ui_->parentCombo->count(); ++i) {
        if (ui_->parentCombo->itemData(i).toString() == targetUuid) {
            ui_->parentCombo->setCurrentIndex(i);
            return;
        }
    }
    ui_->parentCombo->setCurrentIndex(0);
}

}
