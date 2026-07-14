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
#include "ores.qt/CalendarDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/calendar_protocol.hpp"
#include "ui_CalendarDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

CalendarDetailDialog::CalendarDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CalendarDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
}

CalendarDetailDialog::~CalendarDetailDialog() {
    delete ui_;
}

QTabWidget* CalendarDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CalendarDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CalendarDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CalendarDetailDialog::code() const {
    return QString::fromStdString(calendar_.code);
}

void CalendarDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CalendarDetailDialog::setupCombos() {}

void CalendarDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &CalendarDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &CalendarDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &CalendarDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &CalendarDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &CalendarDetailDialog::onFieldChanged);
    connect(ui_->calendarTypeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CalendarDetailDialog::onFieldChanged);
    connect(ui_->countryCodeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CalendarDetailDialog::onFieldChanged);
}

void CalendarDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateCalendarTypeCombo();
    populateCountryCodeCombo();
}

void CalendarDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CalendarDetailDialog::setCalendar(const refdata::domain::calendar& calendar) {
    calendar_ = calendar;
    updateUiFromCalendar();
}

void CalendarDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CalendarDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->calendarTypeCombo->setEnabled(!readOnly);
    ui_->countryCodeCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CalendarDetailDialog::populateCalendarTypeCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating calendar_type combo";
    populateDynamicCombo<refdata::domain::calendar_type>(
        ui_->calendarTypeCombo,
        this,
        clientManager_,
        &fetch_calendar_types,
        "calendarTypeWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(calendar_.calendar_type); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load calendar types: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); });
}
void CalendarDetailDialog::populateCountryCodeCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating country_code combo";
    populateDynamicCombo<refdata::domain::country>(
        ui_->countryCodeCombo,
        this,
        clientManager_,
        &fetch_countries,
        "countryCodeWatcher",
        [](const auto& t) { return QString::fromStdString(t.alpha2_code); },
        [](const auto& t) { return QString::fromStdString(t.name); },
        [](const auto& t) { return t.version; },
        [this]() { return QString::fromStdString(calendar_.country_code); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load countries: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.alpha2_code); });
}
void CalendarDetailDialog::updateUiFromCalendar() {
    ui_->codeEdit->setText(QString::fromStdString(calendar_.code));
    ui_->nameEdit->setText(QString::fromStdString(calendar_.name));
    {
        const auto val = QString::fromStdString(calendar_.calendar_type);
        const int idx = ui_->calendarTypeCombo->findData(val);
        if (idx >= 0)
            ui_->calendarTypeCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(calendar_.country_code);
        const int idx = ui_->countryCodeCombo->findData(val);
        if (idx >= 0)
            ui_->countryCodeCombo->setCurrentIndex(idx);
    }

    populateProvenance(calendar_.version,
                       calendar_.modified_by,
                       calendar_.performed_by,
                       calendar_.recorded_at,
                       calendar_.change_reason_code,
                       calendar_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CalendarDetailDialog::updateCalendarFromUi() {
    if (createMode_) {
        calendar_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    calendar_.name = ui_->nameEdit->text().trimmed().toStdString();
    calendar_.calendar_type = ui_->calendarTypeCombo->currentText().toStdString();
    calendar_.country_code = ui_->countryCodeCombo->currentText().toStdString();
    calendar_.modified_by = username_;
}

void CalendarDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CalendarDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void CalendarDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save calendar while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }


    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    calendar_.change_reason_code = crSel->reason_code;
    calendar_.change_commentary = crSel->commentary;

    updateCalendarFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving calendar: " << calendar_.code;

    QPointer<CalendarDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, calendar = calendar_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_calendar_request request;
        request.data = calendar;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Calendar saved successfully";
            QString code = QString::fromStdString(self->calendar_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->calendarSaved(code);
            self->notifySaveSuccess(tr("Calendar '%1' saved").arg(code));
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

void CalendarDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete calendar while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(calendar_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Calendar",
        QString("Are you sure you want to delete calendar '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting calendar: " << calendar_.code;

    QPointer<CalendarDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = calendar_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_calendar_request request;
        request.codes = {code};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Calendar deleted successfully";
            emit self->statusMessage(QString("Calendar '%1' deleted").arg(code));
            emit self->calendarDeleted(code);
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
