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
#include "ores.qt/CalendarExceptionDetailDialog.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/calendar_exception_protocol.hpp"
#include "ui_CalendarExceptionDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

CalendarExceptionDetailDialog::CalendarExceptionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CalendarExceptionDetailDialog)
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
    // Composite child-entity tables seam: an :implements
    // 7E4A2C8D-9F1B-4E6A-8D3C-5B2A7E9F1C4D block constructs one QTableWidget
    // + QToolBar per embedded child entity (e.g. identifiers, contact
    // information), wraps each in a tab, and inserts it into this dialog's
    // tab widget. Left empty when no entity implements this kind.
}

CalendarExceptionDetailDialog::~CalendarExceptionDetailDialog() {
    delete ui_;
}

QTabWidget* CalendarExceptionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CalendarExceptionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CalendarExceptionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CalendarExceptionDetailDialog::code() const {
    return QString::fromStdString(boost::uuids::to_string(exc_.id));
}

void CalendarExceptionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CalendarExceptionDetailDialog::setupCombos() {}

void CalendarExceptionDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &CalendarExceptionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CalendarExceptionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &CalendarExceptionDetailDialog::onCloseClicked);

    connect(
        ui_->idEdit, &QLineEdit::textChanged, this, &CalendarExceptionDetailDialog::onCodeChanged);
    connect(ui_->calendarCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CalendarExceptionDetailDialog::onFieldChanged);
    connect(ui_->exceptionDateEdit,
            &QLineEdit::textChanged,
            this,
            &CalendarExceptionDetailDialog::onFieldChanged);
    connect(ui_->isBusinessDayCheckBox,
            &QCheckBox::toggled,
            this,
            &CalendarExceptionDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QLineEdit::textChanged,
            this,
            &CalendarExceptionDetailDialog::onFieldChanged);
}

void CalendarExceptionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateCalendarCode();
}

void CalendarExceptionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CalendarExceptionDetailDialog::setException(const refdata::domain::calendar_exception& exc) {
    exc_ = exc;
    updateUiFromException();
}

void CalendarExceptionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(true);
    ui_->calendarCombo->setEnabled(createMode);
    ui_->exceptionDateEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        exc_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void CalendarExceptionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarExceptionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->calendarCombo->setEnabled(false);
    ui_->exceptionDateEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CalendarExceptionDetailDialog::populateCalendarCode() {
    BOOST_LOG_SEV(lg(), debug) << "Populating calendar_code combo";
    populateDynamicCombo<refdata::domain::calendar>(
        ui_->calendarCombo,
        this,
        clientManager_,
        &fetch_calendars,
        "calendarExceptionCalendarWatcher",
        [](const auto& t) { return QString::fromStdString(t.name); },
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return t.version; },
        [this]() { return QString::fromStdString(exc_.calendar_code); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load calendars: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void CalendarExceptionDetailDialog::updateUiFromException() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(exc_.id)));
    {
        const auto val = QString::fromStdString(exc_.calendar_code);
        const int idx = ui_->calendarCombo->findData(val);
        if (idx >= 0)
            ui_->calendarCombo->setCurrentIndex(idx);
    }
    ui_->exceptionDateEdit->setText(QString::fromStdString(
        ores::platform::time::datetime::to_iso8601_date(exc_.exception_date)));
    ui_->isBusinessDayCheckBox->setChecked(exc_.is_business_day);
    ui_->descriptionEdit->setText(exc_.description ? QString::fromStdString(*exc_.description) :
                                                     QString{});

    populateProvenance(exc_.version,
                       exc_.modified_by,
                       exc_.performed_by,
                       exc_.recorded_at,
                       exc_.change_reason_code,
                       exc_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CalendarExceptionDetailDialog::updateExceptionFromUi() {
    if (createMode_) {
        exc_.calendar_code = ui_->calendarCombo->currentData().toString().trimmed().toStdString();
    }
    if (createMode_) {
        exc_.exception_date = ores::platform::time::datetime::from_iso8601_date(
            ui_->exceptionDateEdit->text().trimmed().toStdString());
    }
    exc_.is_business_day = ui_->isBusinessDayCheckBox->isChecked();
    {
        const auto description_str = ui_->descriptionEdit->text().trimmed().toStdString();
        exc_.description =
            description_str.empty() ? std::nullopt : std::optional<std::string>(description_str);
    }
    exc_.modified_by = username_;
}

void CalendarExceptionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarExceptionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarExceptionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CalendarExceptionDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString exception_date_val = ui_->exceptionDateEdit->text().trimmed();
    const bool calendar_code_selected = ui_->calendarCombo->currentIndex() >= 0;
    const QString exception_date_date_val = ui_->exceptionDateEdit->text().trimmed();

    return true && !id_val.isEmpty() && !exception_date_val.isEmpty() &&
           calendar_code_selected
           // A blank optional date field is valid (nothing to parse); a
           // non-blank one must be a real ISO-8601 date, or the unchecked
           // from_iso8601_date() parse in updateXFromUi() would throw and
           // crash the app on Save.
           && (exception_date_date_val.isEmpty() ||
               ores::platform::time::datetime::is_valid_iso8601_date(
                   exception_date_date_val.toStdString()));
}

void CalendarExceptionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save calendar exception while disconnected from server.");
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
    exc_.change_reason_code = crSel->reason_code;
    exc_.change_commentary = crSel->commentary;

    updateExceptionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving calendar exception: " << boost::uuids::to_string(exc_.id);

    QPointer<CalendarExceptionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, exc = exc_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_calendar_exception_request request;
        request.data = exc;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher,
            &QFutureWatcher<SaveResult>::finished,
            self,
            [self, watcher, crReasonCode = crSel->reason_code, crCommentary = crSel->commentary]() {
                auto result = watcher->result();
                watcher->deleteLater();

                if (result.success) {
                    BOOST_LOG_SEV(lg(), info) << "Calendar Exception saved successfully";
                    QString code = QString::fromStdString(boost::uuids::to_string(self->exc_.id));
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->excSaved(code);
                    self->notifySaveSuccess(tr("Calendar Exception '%1' saved").arg(code));
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

void CalendarExceptionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete calendar exception while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(exc_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Calendar Exception",
        QString("Are you sure you want to delete calendar exception '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting calendar exception: "
                              << boost::uuids::to_string(exc_.id);

    QPointer<CalendarExceptionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(exc_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_calendar_exception_request request;
        request.ids = {id_str};
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
            BOOST_LOG_SEV(lg(), info) << "Calendar Exception deleted successfully";
            emit self->statusMessage(QString("Calendar Exception '%1' deleted").arg(code));
            emit self->excDeleted(code);
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
