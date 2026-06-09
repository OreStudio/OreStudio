/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024-2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include "ores.qt/HelpViewer.hpp"
#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QHelpContentWidget>
#include <QHelpEngine>
#include <QHelpEngineCore>
#include <QHelpIndexWidget>
#include <QHelpLink>
#include <QHelpSearchEngine>
#include <QHelpSearchResultWidget>
#include <QLabel>
#include <QLineEdit>
#include <QSplitter>
#include <QStandardPaths>
#include <QTabWidget>
#include <QTextBrowser>
#include <QUrl>
#include <QVBoxLayout>
#include <QWidget>

namespace ores::qt {

using namespace ores::logging;

namespace {

constexpr auto qch_name = "user_manual.qch";

/*
 * A QTextBrowser cannot fetch qthelp:// URLs on its own; loadResource is
 * overridden to read file data out of the QHelpEngine, so links, images
 * and cross-references inside the collection resolve in-process.
 */
class HelpBrowser final : public QTextBrowser {
public:
    HelpBrowser(QHelpEngine* engine, QWidget* parent)
        : QTextBrowser(parent), engine_(engine) {
        setOpenExternalLinks(false);
    }

    QVariant loadResource(int type, const QUrl& url) override {
        if (url.scheme() == "qthelp")
            return engine_->fileData(url);
        return QTextBrowser::loadResource(type, url);
    }

private:
    QHelpEngine* engine_;
};

}

std::optional<QString> HelpViewer::locateHelpCollection() {
    // 1. Explicit override for developers and packagers.
    const auto env = qEnvironmentVariable("ORES_HELP_QCH");
    if (!env.isEmpty() && QFileInfo::exists(env))
        return env;

    // 2. Locations relative to the running executable (install layouts).
    const QString appDir = QCoreApplication::applicationDirPath();
    const QStringList relative{
        QString("%1/%2").arg(appDir, qch_name),
        QString("%1/help/%2").arg(appDir, qch_name),
        QString("%1/../share/orestudio/help/%2").arg(appDir, qch_name),
    };
    for (const auto& path : relative) {
        if (QFileInfo::exists(path))
            return QDir::cleanPath(path);
    }

    // 3. In-tree build output (deploy_help_qch), found by walking up from
    //    the executable to a directory that holds build/output/help.
    QDir dir(appDir);
    for (int i = 0; i < 8; ++i) {
        const QString candidate =
            dir.filePath(QString("build/output/help/%1").arg(qch_name));
        if (QFileInfo::exists(candidate))
            return QDir::cleanPath(candidate);
        if (!dir.cdUp())
            break;
    }
    return std::nullopt;
}

HelpViewer::HelpViewer(QWidget* parent) : QWidget(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Creating help viewer.";

    const auto qch = locateHelpCollection();
    if (!qch) {
        BOOST_LOG_SEV(lg(), warn) << "No help collection (" << qch_name
                                  << ") found; help viewer unavailable.";
        auto* layout = new QVBoxLayout(this);
        layout->addWidget(new QLabel(
            tr("The user manual help collection could not be found.\n"
               "Build it with the deploy_help_qch target."), this));
        return;
    }

    // The collection (.qhc) is the engine's writable index; keep it in the
    // app cache so a read-only install location for the .qch still works.
    const QString cacheDir =
        QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    QDir().mkpath(cacheDir);
    const QString collection = QDir(cacheDir).filePath("user_manual.qhc");

    engine_ = new QHelpEngine(collection, this);
    if (!engine_->setupData()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Help engine setup failed: " << engine_->error().toStdString();
    }

    // Register the .qch into the collection if it is not already present.
    const QString ns = QHelpEngineCore::namespaceName(*qch);
    if (!ns.isEmpty()
        && !engine_->registeredDocumentations().contains(ns)) {
        if (!engine_->registerDocumentation(*qch)) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to register help: "
                                      << engine_->error().toStdString();
        }
    }
    engine_->setupData();

    available_ = true;
    buildUi();
    BOOST_LOG_SEV(lg(), info)
        << "Help viewer ready from " << qch->toStdString();
}

HelpViewer::~HelpViewer() = default;

bool HelpViewer::isAvailable() const { return available_; }

void HelpViewer::buildUi() {
    auto* browser = new HelpBrowser(engine_, this);

    // Sidebar: Contents, Index, Search tabs.
    auto* tabs = new QTabWidget(this);

    auto* contents = engine_->contentWidget();
    tabs->addTab(contents, tr("Contents"));
    connect(contents, &QHelpContentWidget::linkActivated,
            browser, [browser](const QUrl& url) { browser->setSource(url); });

    auto* index = engine_->indexWidget();
    tabs->addTab(index, tr("Index"));
    connect(index, &QHelpIndexWidget::documentActivated, browser,
            [browser](const QHelpLink& doc, const QString&) {
                browser->setSource(doc.url);
            });

    // Search tab: a query field over the full-text search engine, with the
    // result widget below.
    auto* searchTab = new QWidget(this);
    auto* searchLayout = new QVBoxLayout(searchTab);
    searchField_ = new QLineEdit(searchTab);
    searchField_->setPlaceholderText(tr("Search the manual…"));
    auto* search = engine_->searchEngine();
    auto* results = search->resultWidget();
    searchLayout->addWidget(searchField_);
    searchLayout->addWidget(results, 1);
    tabs->addTab(searchTab, tr("Search"));

    connect(searchField_, &QLineEdit::returnPressed,
            this, &HelpViewer::runSearch);
    connect(results, &QHelpSearchResultWidget::requestShowLink,
            browser, [browser](const QUrl& url) { browser->setSource(url); });
    search->reindexDocumentation();

    // Land on the manual's home page so the content pane is never blank.
    const QString home =
        QString("qthelp://org.orestudio.usermanual/manual/user_manual.html");
    browser->setSource(QUrl(home));

    auto* splitter = new QSplitter(Qt::Horizontal, this);
    splitter->addWidget(tabs);
    splitter->addWidget(browser);
    splitter->setStretchFactor(0, 1);
    splitter->setStretchFactor(1, 3);
    splitter->setSizes({240, 720});

    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(splitter);
}

void HelpViewer::runSearch() {
    if (!engine_ || !searchField_)
        return;
    const QString q = searchField_->text().trimmed();
    if (!q.isEmpty())
        engine_->searchEngine()->search(q);
}

}
