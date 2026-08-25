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
#include "ores.qt/AppProvisionerWizard.hpp"
#include "ores.compute.api/domain/app.hpp"
#include "ores.compute.api/domain/app_version.hpp"
#include "ores.compute.api/domain/app_version_platform.hpp"
#include "ores.compute.api/domain/compute_platform.hpp"
#include "ores.compute.api/messaging/app_protocol.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.api/messaging/app_version_platform_protocol.hpp"
#include "ores.compute.api/messaging/platform_protocol.hpp"
#include "ores.compute.client/client/package_publisher.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include <QApplication>
#include <QCheckBox>
#include <QComboBox>
#include <QFileDialog>
#include <QFileInfo>
#include <QFormLayout>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QMessageBox>
#include <QProgressBar>
#include <QPushButton>
#include <QSpinBox>
#include <QTableWidget>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QWizardPage>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <string>
#include <vector>

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& page_lg() {
    static auto instance = make_logger("ores.qt.app_provisioner_page");
    return instance;
}

}

// ── Page 1: Application identity ─────────────────────────────────────────────

class AppIdentityPage : public QWizardPage {
public:
    explicit AppIdentityPage(QWidget* parent = nullptr)
        : QWizardPage(parent) {
        setTitle(tr("Application"));
        setSubTitle(tr("Define the name and description of the new compute application."));

        auto* layout = new QFormLayout(this);

        name_edit_ = new QLineEdit(this);
        name_edit_->setPlaceholderText(tr("e.g. ORE_STUDIO"));
        layout->addRow(tr("Name *"), name_edit_);

        auto* hint = new QLabel(tr("Unique identifier. Use UPPER_SNAKE_CASE."), this);
        hint->setEnabled(false);
        layout->addRow(QString{}, hint);

        description_edit_ = new QTextEdit(this);
        description_edit_->setMaximumHeight(80);
        layout->addRow(tr("Description"), description_edit_);

        connect(name_edit_, &QLineEdit::textChanged, this, &AppIdentityPage::completeChanged);
    }

    bool isComplete() const override {
        return !name_edit_->text().trimmed().isEmpty();
    }

    QString name() const {
        return name_edit_->text().trimmed();
    }
    QString description() const {
        return description_edit_->toPlainText().trimmed();
    }

private:
    QLineEdit* name_edit_;
    QTextEdit* description_edit_;
};

// ── Page 2: Version details ───────────────────────────────────────────────────

class VersionDetailsPage : public QWizardPage {
public:
    explicit VersionDetailsPage(QWidget* parent = nullptr)
        : QWizardPage(parent) {
        setTitle(tr("Version"));
        setSubTitle(tr("Optionally register the first version of this application."));

        auto* layout = new QVBoxLayout(this);

        register_cb_ = new QCheckBox(tr("Register a version now"), this);
        register_cb_->setChecked(true);
        layout->addWidget(register_cb_);

        fields_widget_ = new QWidget(this);
        auto* form = new QFormLayout(fields_widget_);

        wrapper_edit_ = new QLineEdit(fields_widget_);
        wrapper_edit_->setPlaceholderText(tr("e.g. v1.0.0"));
        form->addRow(tr("Wrapper version *"), wrapper_edit_);

        engine_edit_ = new QLineEdit(fields_widget_);
        engine_edit_->setPlaceholderText(tr("e.g. ORE-Studio-7.1"));
        form->addRow(tr("Engine version *"), engine_edit_);

        min_ram_spin_ = new QSpinBox(fields_widget_);
        min_ram_spin_->setRange(64, 1024 * 1024);
        min_ram_spin_->setValue(512);
        min_ram_spin_->setSuffix(tr(" MB"));
        form->addRow(tr("Min RAM *"), min_ram_spin_);

        layout->addWidget(fields_widget_);
        layout->addStretch();

        connect(register_cb_, &QCheckBox::toggled, this, &VersionDetailsPage::on_register_toggled);
        connect(wrapper_edit_, &QLineEdit::textChanged, this, &VersionDetailsPage::completeChanged);
        connect(engine_edit_, &QLineEdit::textChanged, this, &VersionDetailsPage::completeChanged);
    }

    bool isComplete() const override {
        if (!register_cb_->isChecked())
            return true;
        return !wrapper_edit_->text().trimmed().isEmpty() &&
               !engine_edit_->text().trimmed().isEmpty();
    }

    int nextId() const override {
        return register_cb_->isChecked() ? AppProvisionerWizard::kPlatformsPage :
                                           AppProvisionerWizard::kAuditPage;
    }

    bool register_version() const {
        return register_cb_->isChecked();
    }
    QString wrapper_version() const {
        return wrapper_edit_->text().trimmed();
    }
    QString engine_version() const {
        return engine_edit_->text().trimmed();
    }
    int min_ram_mb() const {
        return min_ram_spin_->value();
    }

private slots:
    void on_register_toggled(bool checked) {
        fields_widget_->setEnabled(checked);
        emit completeChanged();
    }

private:
    QCheckBox* register_cb_;
    QWidget* fields_widget_;
    QLineEdit* wrapper_edit_;
    QLineEdit* engine_edit_;
    QSpinBox* min_ram_spin_;
};

// ── Page 3: Platforms ─────────────────────────────────────────────────────────

class PlatformsPage : public QWizardPage {
public:
    explicit PlatformsPage(ClientManager* clientManager, QWidget* parent = nullptr)
        : QWizardPage(parent)
        , client_manager_(clientManager) {
        setTitle(tr("Platforms"));
        setSubTitle(tr("Select the platforms this version supports. At least one required."));

        auto* layout = new QVBoxLayout(this);

        // Dual-list header
        auto* header = new QHBoxLayout;
        header->addWidget(new QLabel(tr("Available"), this));
        header->addStretch();
        header->addWidget(new QLabel(tr("Selected"), this));
        layout->addLayout(header);

        // Lists + button column
        auto* body = new QHBoxLayout;

        available_list_ = new QListWidget(this);
        available_list_->setSelectionMode(QAbstractItemView::ExtendedSelection);
        body->addWidget(available_list_);

        auto* btn_col = new QVBoxLayout;
        btn_col->addStretch();
        add_btn_ = new QPushButton(tr("Add →"), this);
        remove_btn_ = new QPushButton(tr("← Remove"), this);
        add_all_btn_ = new QPushButton(tr("Add All →"), this);
        rem_all_btn_ = new QPushButton(tr("← Rem. All"), this);
        btn_col->addWidget(add_btn_);
        btn_col->addWidget(remove_btn_);
        btn_col->addSpacing(8);
        btn_col->addWidget(add_all_btn_);
        btn_col->addWidget(rem_all_btn_);
        btn_col->addStretch();
        body->addLayout(btn_col);

        selected_list_ = new QListWidget(this);
        selected_list_->setSelectionMode(QAbstractItemView::ExtendedSelection);
        body->addWidget(selected_list_);

        layout->addLayout(body);

        connect(add_btn_, &QPushButton::clicked, this, &PlatformsPage::on_add);
        connect(remove_btn_, &QPushButton::clicked, this, &PlatformsPage::on_remove);
        connect(add_all_btn_, &QPushButton::clicked, this, &PlatformsPage::on_add_all);
        connect(rem_all_btn_, &QPushButton::clicked, this, &PlatformsPage::on_remove_all);
        connect(selected_list_->model(),
                &QAbstractItemModel::rowsInserted,
                this,
                &PlatformsPage::completeChanged);
        connect(selected_list_->model(),
                &QAbstractItemModel::rowsRemoved,
                this,
                &PlatformsPage::completeChanged);
    }

    void initializePage() override {
        available_list_->clear();
        selected_list_->clear();
        all_platforms_.clear();

        compute::messaging::list_platforms_request req;
        const auto resp = client_manager_->process_authenticated_request(std::move(req));
        if (!resp || !resp->success)
            return;

        for (const auto& p : resp->platforms) {
            if (!p.is_active)
                continue;
            all_platforms_.push_back(p);
            auto* item = new QListWidgetItem(QString::fromStdString(p.display_name));
            item->setData(Qt::UserRole, QString::fromStdString(boost::uuids::to_string(p.id)));
            available_list_->addItem(item);
        }
    }

    bool isComplete() const override {
        return selected_list_->count() > 0;
    }

    std::vector<std::string> selected_platform_ids() const {
        std::vector<std::string> ids;
        ids.reserve(selected_list_->count());
        for (int i = 0; i < selected_list_->count(); ++i)
            ids.push_back(selected_list_->item(i)->data(Qt::UserRole).toString().toStdString());
        return ids;
    }

    QStringList selected_platform_names() const {
        QStringList names;
        for (int i = 0; i < selected_list_->count(); ++i)
            names << selected_list_->item(i)->text();
        return names;
    }

    /**
     * @brief Full platform records (id, code, display_name) for every
     * platform currently in the "selected" list, in list order -- what
     * PackageUploadPage needs to build one upload per platform.
     */
    std::vector<compute::domain::compute_platform> selected_platforms() const {
        std::vector<compute::domain::compute_platform> result;
        result.reserve(selected_list_->count());
        for (int i = 0; i < selected_list_->count(); ++i) {
            const auto id_str =
                selected_list_->item(i)->data(Qt::UserRole).toString().toStdString();
            const auto it =
                std::find_if(all_platforms_.begin(), all_platforms_.end(), [&](const auto& p) {
                    return boost::uuids::to_string(p.id) == id_str;
                });
            if (it != all_platforms_.end())
                result.push_back(*it);
        }
        return result;
    }

private slots:
    void on_add() {
        move_selected(available_list_, selected_list_);
    }
    void on_remove() {
        move_selected(selected_list_, available_list_);
    }
    void on_add_all() {
        move_all(available_list_, selected_list_);
    }
    void on_remove_all() {
        move_all(selected_list_, available_list_);
    }

private:
    static void move_selected(QListWidget* from, QListWidget* to) {
        const auto items = from->selectedItems();
        for (auto* item : items) {
            from->takeItem(from->row(item));
            to->addItem(item);
        }
    }
    static void move_all(QListWidget* from, QListWidget* to) {
        while (from->count() > 0)
            to->addItem(from->takeItem(0));
    }

    ClientManager* client_manager_;
    QListWidget* available_list_;
    QListWidget* selected_list_;
    QPushButton* add_btn_;
    QPushButton* remove_btn_;
    QPushButton* add_all_btn_;
    QPushButton* rem_all_btn_;
    std::vector<compute::domain::compute_platform> all_platforms_;
};

// ── Page 4: Package upload ────────────────────────────────────────────────────

/**
 * @brief One upload per selected platform via the shared
 * ores.compute.client package_publisher -- the canonical
 * {app_name}/{version}/{app_name}-{version}-{platform_code}.tar.gz key,
 * replacing the retired single-URI-for-all-platforms shortcut.
 */
class PackageUploadPage : public QWizardPage {
public:
    enum Column { PlatformColumn, FileColumn, BrowseColumn, StatusColumn, ColumnCount };

    explicit PackageUploadPage(const std::string& httpBaseUrl, QWidget* parent = nullptr)
        : QWizardPage(parent)
        , http_base_url_(httpBaseUrl) {
        setTitle(tr("Package"));
        setSubTitle(tr("Upload the engine bundle for each selected platform."));

        auto* layout = new QVBoxLayout(this);

        table_ = new QTableWidget(this);
        table_->setColumnCount(ColumnCount);
        table_->setHorizontalHeaderLabels({tr("Platform"), tr("File"), QString{}, tr("Status")});
        table_->horizontalHeader()->setSectionResizeMode(FileColumn, QHeaderView::Stretch);
        table_->verticalHeader()->setVisible(false);
        table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
        table_->setSelectionMode(QAbstractItemView::NoSelection);
        layout->addWidget(table_);

        auto* upload_row = new QHBoxLayout;
        upload_btn_ = new QPushButton(tr("Upload All"), this);
        progress_bar_ = new QProgressBar(this);
        progress_bar_->setRange(0, 100);
        progress_bar_->setVisible(false);
        upload_row->addWidget(upload_btn_);
        upload_row->addWidget(progress_bar_, 1);
        layout->addLayout(upload_row);

        connect(upload_btn_, &QPushButton::clicked, this, &PackageUploadPage::on_upload_all);
    }

    void initializePage() override {
        auto* wiz = qobject_cast<AppProvisionerWizard*>(wizard());
        if (!wiz)
            return;

        app_name_ = wiz->app_identity_page_->name().toStdString();
        engine_version_ = wiz->version_details_page_->engine_version().toStdString();

        const auto platforms = wiz->platforms_page_->selected_platforms();

        rows_.clear();
        table_->setRowCount(static_cast<int>(platforms.size()));
        for (int i = 0; i < static_cast<int>(platforms.size()); ++i) {
            const auto& p = platforms[i];

            Row row;
            row.platform_id = p.id;
            row.platform_code = p.code;
            rows_.push_back(std::move(row));

            auto* platform_item = new QTableWidgetItem(QString::fromStdString(p.display_name));
            platform_item->setFlags(platform_item->flags() & ~Qt::ItemIsEditable);
            table_->setItem(i, PlatformColumn, platform_item);

            auto* file_item = new QTableWidgetItem(tr("No file selected"));
            file_item->setFlags(file_item->flags() & ~Qt::ItemIsEditable);
            table_->setItem(i, FileColumn, file_item);

            auto* browse_btn = new QPushButton(tr("Browse…"), table_);
            connect(browse_btn, &QPushButton::clicked, this, [this, i]() { on_browse(i); });
            table_->setCellWidget(i, BrowseColumn, browse_btn);

            auto* status_item = new QTableWidgetItem(tr("Pending"));
            status_item->setFlags(status_item->flags() & ~Qt::ItemIsEditable);
            table_->setItem(i, StatusColumn, status_item);
        }
        emit completeChanged();
    }

    bool isComplete() const override {
        if (rows_.empty() || uploading_)
            return false;
        return std::all_of(
            rows_.begin(), rows_.end(), [](const auto& r) { return !r.sha256.empty(); });
    }

    /**
     * @brief One completed upload result per selected platform -- what
     * AppProvisionerWizard::submit() turns into app_version_platform rows.
     */
    struct Row {
        boost::uuids::uuid platform_id{};
        std::string platform_code;
        std::string local_file;
        std::string package_uri;
        std::string sha256;
    };

    const std::vector<Row>& rows() const {
        return rows_;
    }

private slots:
    void on_browse(int row) {
        const QString path =
            QFileDialog::getOpenFileName(this,
                                         tr("Select Engine Package"),
                                         {},
                                         tr("Package Files (*.tar.gz *.zip);;All Files (*)"));
        if (path.isEmpty())
            return;
        rows_[row].local_file = path.toStdString();
        rows_[row].package_uri.clear();
        rows_[row].sha256.clear();
        table_->item(row, FileColumn)->setText(path);
        table_->item(row, StatusColumn)->setText(tr("Ready"));
        emit completeChanged();
    }

    void on_upload_all() {
        if (http_base_url_.empty()) {
            MessageBoxHelper::warning(
                this,
                tr("No Server URL"),
                tr("HTTP base URL is not configured. Cannot upload packages."));
            return;
        }

        uploading_ = true;
        upload_btn_->setEnabled(false);
        progress_bar_->setVisible(true);
        progress_bar_->setValue(0);
        emit completeChanged();
        QApplication::setOverrideCursor(Qt::WaitCursor);

        compute::client::package_publisher publisher(http_base_url_);
        int done = 0;
        for (int i = 0; i < static_cast<int>(rows_.size()); ++i) {
            auto& row = rows_[i];
            if (row.local_file.empty()) {
                table_->item(i, StatusColumn)->setText(tr("Skipped (no file)"));
                continue;
            }
            table_->item(i, StatusColumn)->setText(tr("Uploading…"));
            qApp->processEvents();
            try {
                const auto result = publisher.publish(
                    app_name_, engine_version_, row.platform_code, row.local_file);
                row.package_uri = result.package_uri;
                row.sha256 = result.sha256;
                table_->item(i, StatusColumn)->setText(tr("Uploaded"));
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(page_lg(), error)
                    << "Upload failed for platform " << row.platform_code << ": " << e.what();
                table_->item(i, StatusColumn)
                    ->setText(tr("Failed: %1").arg(QString::fromStdString(e.what())));
            }
            ++done;
            progress_bar_->setValue(
                static_cast<int>(100 * done / std::max<std::size_t>(1, rows_.size())));
        }

        QApplication::restoreOverrideCursor();
        uploading_ = false;
        upload_btn_->setEnabled(true);
        progress_bar_->setVisible(false);
        emit completeChanged();

        if (!isComplete()) {
            MessageBoxHelper::warning(
                this,
                tr("Upload Incomplete"),
                tr("Not every selected platform has an uploaded package yet."));
        }
    }

private:
    std::string http_base_url_;
    std::string app_name_;
    std::string engine_version_;
    bool uploading_{false};

    QTableWidget* table_;
    QPushButton* upload_btn_;
    QProgressBar* progress_bar_;
    std::vector<Row> rows_;
};

// ── Page 5: Audit ─────────────────────────────────────────────────────────────

class AuditPage : public QWizardPage {
public:
    explicit AuditPage(ChangeReasonCache* changeReasonCache, QWidget* parent = nullptr)
        : QWizardPage(parent)
        , change_reason_cache_(changeReasonCache) {
        setTitle(tr("Audit"));
        setSubTitle(tr("Record why this application is being provisioned."));

        auto* layout = new QFormLayout(this);

        reason_combo_ = new QComboBox(this);
        layout->addRow(tr("Change reason *"), reason_combo_);

        commentary_edit_ = new QLineEdit(this);
        layout->addRow(tr("Commentary"), commentary_edit_);

        connect(reason_combo_,
                QOverload<int>::of(&QComboBox::currentIndexChanged),
                this,
                &AuditPage::on_reason_changed);
    }

    void initializePage() override {
        reason_combo_->clear();
        // "system" is the category with applies_to_new reasons (e.g.
        // system.new_record) -- "common" is amend-only (see
        // AppDetailDialog/WorkunitDetailDialog's createMode_ ? "system" :
        // "common" convention). This wizard only ever creates, so it's
        // always "system", never conditional.
        const auto reasons = change_reason_cache_->getReasonsForNew("system");
        for (const auto& r : reasons) {
            reason_combo_->addItem(QString::fromStdString(r.description),
                                   QString::fromStdString(r.code));
            // Track which reasons require commentary
            if (r.requires_commentary)
                requires_commentary_codes_.insert(QString::fromStdString(r.code));
        }
        on_reason_changed(reason_combo_->currentIndex());
    }

    bool isComplete() const override {
        return reason_combo_->currentIndex() >= 0;
    }

    std::string reason_code() const {
        return reason_combo_->currentData().toString().toStdString();
    }

    std::string commentary() const {
        return commentary_edit_->text().trimmed().toStdString();
    }

private slots:
    void on_reason_changed(int /*index*/) {
        const QString code = reason_combo_->currentData().toString();
        const bool req = requires_commentary_codes_.contains(code);
        commentary_edit_->setPlaceholderText(req ? tr("Required for this reason") : tr("Optional"));
        emit completeChanged();
    }

private:
    ChangeReasonCache* change_reason_cache_;
    QComboBox* reason_combo_;
    QLineEdit* commentary_edit_;
    QSet<QString> requires_commentary_codes_;
};

// ── Page 6: Review ────────────────────────────────────────────────────────────

class AppProvisionerReviewPage : public QWizardPage {
public:
    explicit AppProvisionerReviewPage(QWidget* parent = nullptr)
        : QWizardPage(parent) {
        setTitle(tr("Review"));
        setSubTitle(tr("Confirm the details below then click Create."));

        auto* layout = new QVBoxLayout(this);
        summary_ = new QTextEdit(this);
        summary_->setReadOnly(true);
        layout->addWidget(summary_);
    }

    void initializePage() override {
        auto* wiz = qobject_cast<AppProvisionerWizard*>(wizard());
        if (!wiz)
            return;

        auto* id_page = wiz->app_identity_page_;
        auto* ver_page = wiz->version_details_page_;
        auto* plt_page = wiz->platforms_page_;
        auto* pkg_page = wiz->package_upload_page_;
        auto* aud_page = wiz->audit_page_;

        QStringList lines;
        lines << tr("<b>Application</b>");
        lines << tr("  Name: %1").arg(id_page->name());
        if (!id_page->description().isEmpty())
            lines << tr("  Description: %1").arg(id_page->description());
        lines << QString{};

        if (ver_page->register_version()) {
            lines << tr("<b>Version</b>  %1 / %2")
                         .arg(ver_page->wrapper_version(), ver_page->engine_version());
            lines << tr("  Platforms: %1").arg(plt_page->selected_platform_names().join(", "));
            lines << tr("  Min RAM: %1 MB").arg(ver_page->min_ram_mb());
            for (const auto& row : pkg_page->rows()) {
                lines << tr("  Package (%1): %2")
                             .arg(QString::fromStdString(row.platform_code),
                                  row.package_uri.empty() ?
                                      tr("(not uploaded)") :
                                      QString::fromStdString(row.package_uri));
            }
            lines << QString{};
        } else {
            lines << tr("<i>(No version — can be added later)</i>");
            lines << QString{};
        }

        lines << tr("<b>Audit</b>");
        lines << tr("  Reason: %1").arg(QString::fromStdString(aud_page->reason_code()));
        if (!aud_page->commentary().empty())
            lines << tr("  Commentary: %1").arg(QString::fromStdString(aud_page->commentary()));

        summary_->setHtml(lines.join("<br>"));
    }

    bool validatePage() override {
        auto* wiz = qobject_cast<AppProvisionerWizard*>(wizard());
        return wiz && wiz->submit();
    }

private:
    QTextEdit* summary_;
};

// ── Wizard ────────────────────────────────────────────────────────────────────

AppProvisionerWizard::AppProvisionerWizard(ClientManager* clientManager,
                                           ChangeReasonCache* changeReasonCache,
                                           const std::string& httpBaseUrl,
                                           QWidget* parent)
    : QWizard(parent)
    , client_manager_(clientManager)
    , change_reason_cache_(changeReasonCache)
    , http_base_url_(httpBaseUrl)
    , app_id_(boost::uuids::random_generator()())
    , app_version_id_(boost::uuids::random_generator()()) {

    BOOST_LOG_SEV(lg(), info) << "AppProvisionerWizard constructed with http_base_url='"
                              << (http_base_url_.empty() ? "(empty)" : http_base_url_) << "'";

    setWindowTitle(tr("New Application"));
    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage);
    setButtonText(QWizard::FinishButton, tr("Create"));
    setMinimumSize(600, 480);

    app_identity_page_ = new AppIdentityPage(this);
    version_details_page_ = new VersionDetailsPage(this);
    platforms_page_ = new PlatformsPage(clientManager, this);
    package_upload_page_ = new PackageUploadPage(http_base_url_, this);
    audit_page_ = new AuditPage(changeReasonCache, this);
    app_provisioner_review_page_ = new AppProvisionerReviewPage(this);

    setPage(kAppIdentityPage, app_identity_page_);
    setPage(kVersionDetailsPage, version_details_page_);
    setPage(kPlatformsPage, platforms_page_);
    setPage(kPackageUploadPage, package_upload_page_);
    setPage(kAuditPage, audit_page_);
    setPage(kReviewPage, app_provisioner_review_page_);

    setStartId(kAppIdentityPage);
}

bool AppProvisionerWizard::submit() {
    QApplication::setOverrideCursor(Qt::WaitCursor);

    const std::string reason_code = audit_page_->reason_code();
    const std::string commentary = audit_page_->commentary();
    // storedUsername() is the raw login string (e.g.
    // "tenant_admin@acme_corporation", used for re-auth); modified_by/
    // performed_by need the resolved bare account username the DB's
    // ores_iam_validate_account_username_fn() actually accepts.
    const std::string username = client_manager_->currentUsername();

    // ── Save app ──────────────────────────────────────────────────────────────
    compute::domain::app app;
    app.id = app_id_;
    app.name = app_identity_page_->name().toStdString();
    app.description = app_identity_page_->description().toStdString();
    app.modified_by = username;
    app.performed_by = username;
    app.change_reason_code = reason_code;
    app.change_commentary = commentary;

    compute::messaging::save_app_request app_req;
    app_req.data = std::move(app);

    BOOST_LOG_SEV(lg(), info) << "Provisioning app: " << app_req.data.name;

    const auto app_resp = client_manager_->process_authenticated_request(std::move(app_req));

    QApplication::restoreOverrideCursor();

    if (!app_resp || !app_resp->success) {
        const QString msg =
            app_resp ? QString::fromStdString(app_resp->message) : tr("No response from server");
        BOOST_LOG_SEV(lg(), error) << "App save failed: " << msg.toStdString();
        MessageBoxHelper::critical(this, tr("Create Failed"), msg);
        return false;
    }

    // ── Save app_version (optional) ───────────────────────────────────────────
    if (version_details_page_->register_version()) {
        QApplication::setOverrideCursor(Qt::WaitCursor);

        compute::domain::app_version ver;
        ver.id = app_version_id_;
        ver.app_id = app_id_;
        ver.wrapper_version = version_details_page_->wrapper_version().toStdString();
        ver.engine_version = version_details_page_->engine_version().toStdString();
        ver.min_ram_mb = version_details_page_->min_ram_mb();
        ver.modified_by = username;
        ver.performed_by = username;
        ver.change_reason_code = reason_code;
        ver.change_commentary = commentary;

        // One package_uri/sha256 per platform, from the per-platform uploads
        // performed on PackageUploadPage.
        std::vector<compute::domain::app_version_platform> platform_rows;
        for (const auto& uploaded : package_upload_page_->rows()) {
            compute::domain::app_version_platform row;
            row.app_version_id = app_version_id_;
            row.platform_id = uploaded.platform_id;
            row.platform_code = uploaded.platform_code;
            row.package_uri = uploaded.package_uri;
            row.sha256 = uploaded.sha256;
            platform_rows.push_back(std::move(row));
        }

        compute::messaging::save_app_version_request ver_req;
        ver_req.data = std::move(ver);

        BOOST_LOG_SEV(lg(), info) << "Provisioning app_version: "
                                  << ver_req.data.wrapper_version;

        const auto ver_resp = client_manager_->process_authenticated_request(std::move(ver_req));

        QApplication::restoreOverrideCursor();

        if (!ver_resp || !ver_resp->success) {
            const QString msg = ver_resp ? QString::fromStdString(ver_resp->message) :
                                           tr("No response from server");
            BOOST_LOG_SEV(lg(), error) << "App version save failed: " << msg.toStdString();
            MessageBoxHelper::critical(this, tr("Version Create Failed"), msg);
            return false;
        }

        // Platforms are a junction: swap the version's full platform row set
        // through the junction's replace-by-app-version flow
        // (compute.v1.app_version_platforms.replace_by_app_version_id).
        if (!platform_rows.empty()) {
            compute::messaging::replace_app_version_platforms_by_app_version_request plat_req;
            plat_req.app_version_id = boost::uuids::to_string(app_version_id_);
            plat_req.app_version_platforms = std::move(platform_rows);
            plat_req.modified_by = username;
            plat_req.performed_by = username;
            plat_req.change_reason_code = reason_code;
            plat_req.change_commentary = commentary;

            const auto plat_resp =
                client_manager_->process_authenticated_request(std::move(plat_req));
            if (!plat_resp || !plat_resp->success) {
                const QString msg = plat_resp ? QString::fromStdString(plat_resp->message) :
                                                tr("No response from server");
                BOOST_LOG_SEV(lg(), error) << "App version platforms save failed: "
                                           << msg.toStdString();
                MessageBoxHelper::critical(this, tr("Version Create Failed"), msg);
                return false;
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Application provisioned successfully.";
    emit provisioned();
    return true;
}

} // namespace ores::qt
